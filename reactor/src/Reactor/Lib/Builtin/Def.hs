module Reactor.Lib.Builtin.Def where

import Reactor.Eval (Eval, defineVarEval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

def :: [IR Eval] -> Eval (Maybe (IR Eval))
def [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure Nothing
def _ = throwError $ WrongArgumentType ["symbol", "value"]
