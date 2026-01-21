module Glue.Lib.Builtin.Error where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (runtimeException, wrongArgumentType)
import Glue.IR (IR (..))

errorFunc :: IR Eval
errorFunc = Special errorFuncImpl

errorFuncImpl :: [IR Eval] -> Eval (IR Eval)
errorFuncImpl [Symbol name, rawVal] = do
    val <- eval rawVal
    throwError $ runtimeException name val
errorFuncImpl _ = throwError $ wrongArgumentType ["symbol", "value"]
