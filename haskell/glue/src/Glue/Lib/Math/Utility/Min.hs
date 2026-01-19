module Glue.Lib.Math.Utility.Min where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Minimum function
-- Mirrors Haskell Glue.Lib.Math.Utility.Min.min exactly
min :: IR Eval
min = NativeFunc minImpl

-- Minimum function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Min.minImpl exactly
minImpl :: IR Eval -> Eval (IR Eval)
minImpl arg1 = pure $ NativeFunc (minWith arg1)

minWith :: IR Eval -> IR Eval -> Eval (IR Eval)
minWith arg1 arg2 = do
    va1 <- eval arg1
    va2 <- eval arg2
    case (va1, va2) of
        (Integer n1, Integer n2) -> pure $ Integer (Prelude.min n1 n2)
        (Float n1, Float n2) -> pure $ Float (Prelude.min n1 n2)
        (Integer n1, Float n2) -> pure $ Float (Prelude.min (fromIntegral n1) n2)
        (Float n1, Integer n2) -> pure $ Float (Prelude.min n1 (fromIntegral n2))
        _ -> throwError $ wrongArgumentType ["number", "number"]
