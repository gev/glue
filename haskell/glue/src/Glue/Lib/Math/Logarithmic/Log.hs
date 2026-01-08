module Glue.Lib.Math.Logarithmic.Log where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

log :: [IR Eval] -> Eval (IR Eval)
log [arg, base] = do
    va <- evalRequired arg
    vb <- evalRequired base
    case (va, vb) of
        (Integer n, Integer b) -> pure $ Float (logBase (fromIntegral b) (fromIntegral n))
        (Integer n, Float b) -> pure $ Float (logBase b (fromIntegral n))
        (Float n, Integer b) -> pure $ Float (logBase (fromIntegral b) n)
        (Float n, Float b) -> pure $ Float (logBase b n)
        _ -> throwError $ wrongArgumentType ["number", "number"]
log _ = throwError wrongNumberOfArguments
