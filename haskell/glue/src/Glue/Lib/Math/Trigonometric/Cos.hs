module Glue.Lib.Math.Trigonometric.Cos where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Cosine function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Cos.cos exactly
cos :: IR Eval
cos = NativeFunc cosImpl

-- Cosine function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Cos.cosImpl exactly
cosImpl :: IR Eval -> Eval (IR Eval)
cosImpl arg = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.cos (fromIntegral n))
        Float n -> pure $ Float (Prelude.cos n)
        _ -> throwError $ wrongArgumentType ["number"]
