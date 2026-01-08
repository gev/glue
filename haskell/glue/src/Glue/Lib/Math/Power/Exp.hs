module Glue.Lib.Math.Power.Exp where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

exp :: [IR Eval] -> Eval (IR Eval)
exp [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Float (Prelude.exp (fromIntegral n))
        Float n -> pure $ Float (Prelude.exp n)
        _ -> throwError $ wrongArgumentType ["number"]
exp _ = throwError wrongNumberOfArguments
