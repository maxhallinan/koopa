module Lang.Interpret (InterpretErr, interpretMany) where

import Prelude

import Coroutine (runGenerator)
import Data.Either (Either(..))
import Data.List (List)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Lang.Core (Bindings, EvalErr, EvalState(..), ExprAnn, PrimFns, runEvalT)
import Lang.Eval (eval)
import Lang.Parser (ParseErr, parseMany)

data InterpretErr
  = ParseErr ParseErr
  | EvalErr EvalErr

instance showInterpretErr :: Show InterpretErr where
  show (ParseErr parseErr) = show parseErr
  show (EvalErr evalErr) = show evalErr

interpretMany
  :: forall m
   . Monad m
  => Bindings (PrimFns m)
  -> String
  -> m (Either InterpretErr (List ExprAnn))
interpretMany bindings src =
  case parseMany src of
    Left e ->
      pure $ Left $ ParseErr e
    Right x -> do
      Tuple result _ <- runEvalT (EvalState { bindings }) (runGenerator (traverse eval x))
      pure $ Right result
