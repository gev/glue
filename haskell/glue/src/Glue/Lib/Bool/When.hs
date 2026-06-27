module Glue.Lib.Bool.When where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..), isTruthy)

when_ :: IR Eval
when_ = Special whenImpl

whenImpl :: [IR Eval] -> Eval (IR Eval)
whenImpl [cond, body] = do
    condVal <- eval cond
    if isTruthy condVal
        then eval body
        else pure Void
whenImpl _ = throwError $ wrongArgumentType ["condition", "body"]
