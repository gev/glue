module Glue.Lib.Math.Trigonometric.Atan where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

atan :: [IR Eval] -> Eval (IR Eval)
atan [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.atan (toRealFloat n)))
        _ -> throwError $ wrongArgumentType ["number"]
atan _ = throwError wrongNumberOfArguments
