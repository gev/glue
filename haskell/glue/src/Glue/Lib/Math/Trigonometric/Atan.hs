module Glue.Lib.Math.Trigonometric.Atan where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Arctangent function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Atan.atan exactly
atan :: IR Eval
atan = NativeFunc atanImpl

-- Arctangent function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Atan.atanImpl exactly
atanImpl :: IR Eval -> Eval (IR Eval)
atanImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.atan (fromIntegral n))
    Float n -> pure $ Float (Prelude.atan n)
    _ -> throwError $ wrongArgumentType ["number"]
