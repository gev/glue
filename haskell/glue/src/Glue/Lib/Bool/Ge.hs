module Glue.Lib.Bool.Ge where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

ge :: [IR Eval] -> Eval (IR Eval)
ge [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Integer na, Integer nb) -> pure . Bool $ na >= nb
        (Float na, Float nb) -> pure . Bool $ na >= nb
        (Integer na, Float nb) -> pure . Bool $ fromIntegral na >= nb
        (Float na, Integer nb) -> pure . Bool $ na >= fromIntegral nb
        _ -> throwError $ wrongArgumentType ["number", "number"]
ge _ = throwError $ wrongArgumentType ["number", "number"]
