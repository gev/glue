module Glue.Lib.Math.Trigonometric.Sin where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Sine function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Sin.sin exactly
sin :: IR Eval
sin = NativeFunc sinImpl

-- Sine function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Sin.sinImpl exactly
sinImpl :: IR Eval -> Eval (IR Eval)
sinImpl arg = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.sin (fromIntegral n))
        Float n -> pure $ Float (Prelude.sin n)
        _ -> throwError $ wrongArgumentType ["number"]
