module Glue.Lib.Bool.Gt where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

gt :: [IR Eval] -> Eval (IR Eval)
gt [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure . Bool $ na > nb
        _ -> throwError $ wrongArgumentType ["number", "number"]
gt _ = throwError $ wrongArgumentType ["number", "number"]
