module Glue.Lib.Math.Power.Exp where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Exponential function (e^x)
-- Mirrors Haskell Glue.Lib.Math.Power.Exp.exp exactly
exp :: IR Eval
exp = NativeFunc expImpl

-- Exponential function implementation
-- Mirrors Haskell Glue.Lib.Math.Power.Exp.expImpl exactly
expImpl :: [IR Eval] -> Eval (IR Eval)
expImpl [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.exp (fromIntegral n))
        Float n -> pure $ Float (Prelude.exp n)
        _ -> throwError $ wrongArgumentType ["number"]
expImpl _ = throwError wrongNumberOfArguments
