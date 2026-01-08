module Glue.Lib.Math.Trigonometric.Sin where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

sin :: [IR Eval] -> Eval (IR Eval)
sin [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Float (Prelude.sin (fromIntegral n))
        Float n -> pure $ Float (Prelude.sin n)
        _ -> throwError $ wrongArgumentType ["number"]
sin _ = throwError $ wrongArgumentType ["number"]
