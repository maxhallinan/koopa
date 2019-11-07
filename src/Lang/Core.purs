module Lang.Core where

import Prelude

import Control.Monad.State as S
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Coroutine (CoroutineT, Yield)
import Data.Foldable (intercalate)
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

type Eval m = EvalCoroutine (EvalState (Bindings (PrimFns m))) m

type EvalCoroutine s m = CoroutineT (Yield Unit (LangEffect s)) (EvalT s m)

newtype EvalT s m a = EvalT (StateT s m a)

derive instance newtypeEvalT :: Newtype (EvalT s m a) _
derive newtype instance functorEvalT :: Functor m => Functor (EvalT s m)
derive newtype instance applyEvalT :: Monad m => Apply (EvalT s m)
derive newtype instance applicativeEvalT :: Monad m => Applicative (EvalT s m)
derive newtype instance bindEvalT :: Monad m => Bind (EvalT s m)
derive newtype instance monadEvalT :: Monad m => Monad (EvalT s m)
derive newtype instance monadStateEvalT :: Monad m => MonadState s (EvalT s m)
derive newtype instance monadTransEvalT :: MonadTrans (EvalT s)
derive newtype instance monadEffect :: MonadEffect m => MonadEffect (EvalT s m)
derive newtype instance monadAff :: MonadAff m => MonadAff (EvalT s m)

runEvalT
  :: forall s m a
   . Monad m
  => s
  -> EvalT s m a
  -> m (Tuple a s)
runEvalT evalState = flip runStateT evalState <<< unwrap

data LangEffect s
  = Breakpoint Ann s
  | Console ConsoleEffect s
  | Throw EvalErr

data ConsoleEffect
  = ConsoleLog (List ExprAnn) SrcSpan

data EvalErr = EvalErr ErrTipe SrcSpan

instance showEvalErr :: Show EvalErr where
  show (EvalErr errTipe _) =
    case errTipe of
      NumArgs _ ->
        "wrong number of arguments"
      WrongTipe _ ->
        "wrong type"
      LstLength ctx ->
        "wrong list length " <> (show $ ctx.received)
      UnknownVar { varName } ->
        "unknown identifier " <> varName
      EmptyFnApplication ->
        "empty list in function application position"
      UnknownErr ->
        "unknown error"
      TrueCondClauseNotFound ->
        "cond found no true predicates. Add an `else` clause at the end."

data ErrTipe
  = NumArgs { expected :: Int, received :: Int }
  | WrongTipe { expected :: ExprTipe, received :: ExprTipe }
  | LstLength { expected :: Int, received :: Int }
  | UnknownVar { varName :: String }
  | EmptyFnApplication
  | UnknownErr
  | TrueCondClauseNotFound

newtype EvalState bindings = EvalState { bindings :: bindings }

derive instance evalStateNewtype :: Newtype (EvalState a) _

getEnv :: forall r m. Monad m => EvalT (EvalState (Bindings r)) m Env
getEnv = S.get >>= (unwrap >>> _.bindings >>> unwrap >>> _.env >>> pure)

updateEnv :: forall m. Monad m => String -> ExprAnn -> EvalT (EvalState (Bindings (PrimFns m))) m Unit
updateEnv key val =
  S.modify_ \(EvalState s@({ bindings: Bindings b })) ->
    EvalState $ s { bindings = Bindings b { env = M.insert key val b.env } }

data ExprAnn = ExprAnn Expr Ann

derive instance exprAnnEq :: Eq ExprAnn

instance showExprAnn :: Show ExprAnn where
  show (ExprAnn expr _) = show expr

type Ann = { srcSpan :: SrcSpan }

type SrcSpan = { begin :: SrcLoc, end :: SrcLoc }

type SrcLoc = { line :: Int, column :: Int }

data Expr
  = Sym String
  | SFrm SFrm
  | Fn Env (List ExprAnn) ExprAnn
  | Lst (List ExprAnn)
  | Integer Int
  | Float Number
derive instance eqExpr :: Eq Expr

instance showExpr :: Show Expr where
  show (Sym name) = name
  show (SFrm sfrm) = show sfrm
  show (Fn _ _ _) = "<function>"
  show (Float n) = show n
  show (Integer n) = show n
  show (Lst exprs) = "(" <> exprs' <> ")"
    where exprs' = intercalate " " $ map show exprs

type Env = Map String ExprAnn

newtype Bindings primFns = Bindings
  { env :: Env
  , primFns :: primFns
  }

derive instance newtypeBindings :: Newtype (Bindings a) _

derive newtype instance semigroupBindings :: Semigroup a => Semigroup (Bindings a)

derive newtype instance monoidBindings :: Monoid a => Monoid (Bindings a)

newtype PrimFns m = PrimFns (Map String (List ExprAnn -> Eval m ExprAnn))

instance semigroupPrimFns :: Semigroup (PrimFns m) where
  append (PrimFns p1) (PrimFns p2) = PrimFns (p1 <> p2)

instance monoidPrimFns :: Monoid (PrimFns m) where
  mempty = PrimFns mempty

data SFrm
  = Car
  | Cdr
  | Cond
  | Conz
  | Def
  | Do
  | If
  | IsAtm
  | IsEq
  | Lambda
  | Pause
  | Print
  | Quote

derive instance eqSFrm :: Eq SFrm

instance showSFrm :: Show SFrm where
  show Car = "car"
  show Cdr = "cdr"
  show Cond = "cond"
  show Conz = "cons"
  show If = "if"
  show Def = "define"
  show Do = "do"
  show IsAtm = "atom?"
  show IsEq = "equal?"
  show Lambda = "lambda"
  show Pause = "pause"
  show Print = "print"
  show Quote = "quote"

sfrmNumArgs :: SFrm -> Int
sfrmNumArgs Car = 1
sfrmNumArgs Cdr = 1
sfrmNumArgs Cond = 1
sfrmNumArgs Conz = 2
sfrmNumArgs Def = 1
sfrmNumArgs Do = 1
sfrmNumArgs If = 3
sfrmNumArgs IsAtm = 1
sfrmNumArgs IsEq = 1
sfrmNumArgs Lambda = 2
sfrmNumArgs Pause = 0
sfrmNumArgs Print = 1
sfrmNumArgs Quote = 1

data ExprTipe
  = SymTipe
  | LstTipe
  | FnTipe
  | SFrmTipe
  | FloatTipe
  | IntegerTipe

toExprTipe :: ExprAnn -> ExprTipe
toExprTipe (ExprAnn expr _ ) =
  case expr of
    Sym _ ->
      SymTipe
    Lst _ ->
      LstTipe
    Fn _ _ _ ->
      FnTipe
    SFrm _ ->
      SFrmTipe
    Float _ ->
      FloatTipe
    Integer _ ->
      IntegerTipe

mkTrue :: Ann -> ExprAnn
mkTrue ann = ExprAnn (Sym "true") ann

isTrue :: ExprAnn -> Boolean
isTrue (ExprAnn (Lst L.Nil) _) = false
isTrue (ExprAnn (Sym "false") _) = false
isTrue _ = true

mkFalse :: Ann -> ExprAnn
mkFalse ann = ExprAnn (Sym "false") ann

isFalse :: ExprAnn -> Boolean
isFalse = not <<< isTrue
