module Glue.Lib.Builtin.Error where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (runtimeException, wrongArgumentType)
import Glue.IR (IR (..))

errorFunc :: [IR Eval] -> Eval (IR Eval)
errorFunc [Symbol name, rawVal] = do
    val <- eval rawVal
    throwError $ runtimeException name val
errorFunc _ = throwError $ wrongArgumentType ["symbol", "value"]
