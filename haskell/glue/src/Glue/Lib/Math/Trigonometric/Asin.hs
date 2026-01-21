module Glue.Lib.Math.Trigonometric.Asin where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Arcsine function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asin exactly
asin :: IR Eval
asin = NativeFunc asinImpl

-- Arcsine function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asinImpl exactly
asinImpl :: IR Eval -> Eval (IR Eval)
asinImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.asin (fromIntegral n))
    Float n -> pure $ Float (Prelude.asin n)
    _ -> throwError $ wrongArgumentType ["number"]
