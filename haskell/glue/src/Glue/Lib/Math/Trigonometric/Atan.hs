module Glue.Lib.Math.Trigonometric.Atan where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

atan :: [IR Eval] -> Eval (IR Eval)
atan [arg] = do
    va <- evalRequired arg
    case va of
        Integer n -> pure $ Float (Prelude.atan (fromIntegral n))
        Float n -> pure $ Float (Prelude.atan n)
        _ -> throwError $ wrongArgumentType ["number"]
atan _ = throwError wrongNumberOfArguments
