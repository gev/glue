module Glue.Lib.Math.Trigonometric.Cos where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

cos :: [IR Eval] -> Eval (IR Eval)
cos [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Float (Prelude.cos (fromIntegral n))
        Float n -> pure $ Float (Prelude.cos n)
        _ -> throwError $ wrongArgumentType ["number"]
cos _ = throwError wrongNumberOfArguments
