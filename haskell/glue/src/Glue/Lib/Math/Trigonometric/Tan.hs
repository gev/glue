module Glue.Lib.Math.Trigonometric.Tan where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Tangent function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Tan.tan exactly
tan :: IR Eval
tan = NativeFunc tanImpl

-- Tangent function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Tan.tanImpl exactly
tanImpl :: IR Eval -> Eval (IR Eval)
tanImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.tan (fromIntegral n))
    Float n -> pure $ Float (Prelude.tan n)
    _ -> throwError $ wrongArgumentType ["number"]
