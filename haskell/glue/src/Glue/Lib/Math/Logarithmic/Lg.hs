module Glue.Lib.Math.Logarithmic.Lg where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Common logarithm function (lg, log base 10)
-- Mirrors Haskell Glue.Lib.Math.Logarithmic.Lg.lg exactly
lg :: IR Eval
lg = NativeFunc lgImpl

-- Common logarithm function implementation
-- Mirrors Haskell Glue.Lib.Math.Logarithmic.Lg.lgImpl exactly
lgImpl :: IR Eval -> Eval (IR Eval)
lgImpl arg = case arg of
    Integer n -> pure $ Float (logBase 10 (fromIntegral n))
    Float n -> pure $ Float (logBase 10 n)
    _ -> throwError $ wrongArgumentType ["number"]
