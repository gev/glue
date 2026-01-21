module Glue.Lib.Math.Trigonometric.Acos where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Arccosine function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Acos.acos exactly
acos :: IR Eval
acos = NativeFunc acosImpl

-- Arccosine function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Acos.acosImpl exactly
acosImpl :: IR Eval -> Eval (IR Eval)
acosImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.acos (fromIntegral n))
    Float n -> pure $ Float (Prelude.acos n)
    _ -> throwError $ wrongArgumentType ["number"]
