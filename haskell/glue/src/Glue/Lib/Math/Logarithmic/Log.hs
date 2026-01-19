module Glue.Lib.Math.Logarithmic.Log where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Logarithm function (log base value)
-- Mirrors Haskell Glue.Lib.Math.Logarithmic.Log.log exactly
log :: IR Eval
log = NativeFunc logImpl

-- Logarithm function implementation
-- Mirrors Haskell Glue.Lib.Math.Logarithmic.Log.logImpl exactly
logImpl :: IR Eval -> Eval (IR Eval)
logImpl arg = pure $ NativeFunc (logWithBase arg)

logWithBase :: IR Eval -> IR Eval -> Eval (IR Eval)
logWithBase arg base = do
    va <- eval arg
    vb <- eval base
    case (va, vb) of
        (Integer n, Integer b) -> pure $ Float (Prelude.logBase (fromIntegral b) (fromIntegral n))
        (Integer n, Float b) -> pure $ Float (Prelude.logBase b (fromIntegral n))
        (Float n, Integer b) -> pure $ Float (Prelude.logBase (fromIntegral b) n)
        (Float n, Float b) -> pure $ Float (Prelude.logBase b n)
        _ -> throwError $ wrongArgumentType ["number", "number"]
