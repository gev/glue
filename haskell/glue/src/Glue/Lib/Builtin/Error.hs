module Glue.Lib.Builtin.Error where

import Glue.Eval (Eval, defineVarEval, eval, throwError)
import Glue.Eval.Exception (runtimeException, wrongArgumentType)
import Glue.IR (IR (..))

errorFunc :: [IR Eval] -> Eval (IR Eval)
errorFunc [Symbol name, rawVal] = do
    val <- eval rawVal
    defineVarEval name val
    throwError $ runtimeException name val
errorFunc _ = throwError $ wrongArgumentType ["symbol", "value"]
