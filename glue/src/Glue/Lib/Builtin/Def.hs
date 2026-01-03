module Glue.Lib.Builtin.Def where

import Glue.Eval (Eval, defineVarEval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

def :: [IR Eval] -> Eval (Maybe (IR Eval))
def [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    pure Nothing
def _ = throwError $ WrongArgumentType ["symbol", "value"]
