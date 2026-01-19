module Glue.Lib.Math.Power.Sqrt where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Square root function
-- Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrt exactly
sqrt :: IR Eval
sqrt = NativeFunc sqrtImpl

-- Square root function implementation
-- Mirrors Haskell Glue.Lib.Math.Power.Sqrt.sqrtImpl exactly
sqrtImpl :: IR Eval -> Eval (IR Eval)
sqrtImpl arg = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.sqrt (fromIntegral n))
        Float n -> pure $ Float (Prelude.sqrt n)
        _ -> throwError $ wrongArgumentType ["number"]
