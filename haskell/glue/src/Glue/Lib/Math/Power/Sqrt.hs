module Glue.Lib.Math.Power.Sqrt where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

sqrt :: [IR Eval] -> Eval (IR Eval)
sqrt [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Float (Prelude.sqrt (fromIntegral n))
        Float n -> pure $ Float (Prelude.sqrt n)
        _ -> throwError $ wrongArgumentType ["number"]
sqrt _ = throwError wrongNumberOfArguments
