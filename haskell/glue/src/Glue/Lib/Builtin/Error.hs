module Glue.Lib.Builtin.Error where

import Glue.Eval (Eval, defineVarEval, evalRequired, throwError)
import Glue.Eval.Exception (runtimeException, wrongArgumentType)
import Glue.IR (IR (..))

errorFunc :: [IR Eval] -> Eval (IR Eval)
errorFunc [Symbol name, rawVal] = do
    val <- evalRequired rawVal
    defineVarEval name val
    throwError $ runtimeException name val
errorFunc _ = throwError $ wrongArgumentType ["symbol", "value"]
