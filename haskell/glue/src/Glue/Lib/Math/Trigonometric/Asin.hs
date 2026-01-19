module Glue.Lib.Math.Trigonometric.Asin where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Arcsine function
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asin exactly
asin :: IR Eval
asin = NativeFunc asinImpl

-- Arcsine function implementation
-- Mirrors Haskell Glue.Lib.Math.Trigonometric.Asin.asinImpl exactly
asinImpl :: [IR Eval] -> Eval (IR Eval)
asinImpl [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.asin (fromIntegral n))
        Float n -> pure $ Float (Prelude.asin n)
        _ -> throwError $ wrongArgumentType ["number"]
asinImpl _ = throwError wrongNumberOfArguments
