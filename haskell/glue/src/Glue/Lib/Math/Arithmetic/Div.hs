module Glue.Lib.Math.Arithmetic.Div where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Division function
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.div exactly
div :: IR Eval
div = NativeFunc divImpl

-- Division function implementation
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.divImpl exactly
divImpl :: IR Eval -> Eval (IR Eval)
divImpl left = pure $ NativeFunc (divBy left)

divBy :: IR Eval -> IR Eval -> Eval (IR Eval)
divBy left right = case (left, right) of
    (Integer a, Integer b) -> pure $ Float (fromIntegral a / fromIntegral b)
    (Integer a, Float b) -> pure $ Float (fromIntegral a / b)
    (Float a, Integer b) -> pure $ Float (a / fromIntegral b)
    (Float a, Float b) -> pure $ Float (a / b)
    _ -> throwError $ wrongArgumentType ["number"]
