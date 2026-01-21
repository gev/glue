module Glue.Lib.Math.Trigonometric.Sin where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Sine function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Sin.sin exactly
sin :: IR Eval
sin = NativeFunc sinImpl

-- Sine function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Sin.sinImpl exactly
sinImpl :: IR Eval -> Eval (IR Eval)
sinImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.sin (fromIntegral n))
    Float n -> pure $ Float (Prelude.sin n)
    _ -> throwError $ wrongArgumentType ["number"]
