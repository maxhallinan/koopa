module ExprAnn
  ( Ann(..)
  , Env
  , Expr(..)
  , ExprAnn(..)
  , ExprTipe(..)
  , SrcLoc(..)
  , SrcSpan(..)
  , SFrm(..)
  , sfrmNumArgs
  , toExprTipe
  ) where

import Prelude
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Map (Map)

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

data SFrm
  = First
  | Rest
  | Cons
  | If
  | Def
  | IsAtm
  | IsEq
  | Lambda
  | Quote
derive instance eqSFrm :: Eq SFrm

instance showSFrm :: Show SFrm where
  show First = "first"
  show Rest = "rest"
  show Cons = "cons"
  show If = "if"
  show Def = "label"
  show IsAtm = "atom?"
  show IsEq = "equal?"
  show Lambda = "fn"
  show Quote = "quote"

sfrmNumArgs :: SFrm -> Int
sfrmNumArgs Cons = 2
sfrmNumArgs Def = 1
sfrmNumArgs First = 1
sfrmNumArgs If = 3
sfrmNumArgs IsAtm = 1
sfrmNumArgs IsEq = 1
sfrmNumArgs Lambda = 2
sfrmNumArgs Quote = 1
sfrmNumArgs Rest = 1

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
