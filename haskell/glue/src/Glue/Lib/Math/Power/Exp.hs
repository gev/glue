module Glue.Lib.Math.Power.Exp where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Exponential function (e^x)
-- Mirrors Haskell Glue.Lib.Math.Power.Exp.exp exactly
exp :: IR Eval
exp = NativeFunc expImpl

-- Exponential function implementation
-- Mirrors Haskell Glue.Lib.Math.Power.Exp.expImpl exactly
expImpl :: IR Eval -> Eval (IR Eval)
expImpl arg = case arg of
    Integer n -> pure $ Float (Prelude.exp (fromIntegral n))
    Float n -> pure $ Float (Prelude.exp n)
    _ -> throwError $ wrongArgumentType ["number"]
