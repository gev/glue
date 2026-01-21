module Glue.Lib.Math.Power.Sqrt where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Square root function
-- Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrt exactly
sqrt :: IR Eval
sqrt = NativeFunc sqrtImpl

-- Square root function implementation
-- Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrtImpl exactly
sqrtImpl :: IR Eval -> Eval (IR Eval)
sqrtImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.sqrt (fromIntegral n))
    Float n -> pure $ Float (Prelude.sqrt n)
    _ -> throwError $ wrongArgumentType ["number"]
