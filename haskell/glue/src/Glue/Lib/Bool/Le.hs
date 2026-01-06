module Glue.Lib.Bool.Le where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

le :: [IR Eval] -> Eval (IR Eval)
le [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure . Bool $ na <= nb
        _ -> throwError $ wrongArgumentType ["number", "number"]
le _ = throwError $ wrongArgumentType ["number", "number"]
