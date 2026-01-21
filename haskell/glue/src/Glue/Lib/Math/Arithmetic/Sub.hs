module Glue.Lib.Math.Arithmetic.Sub where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Subtraction function
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Sub.sub exactly
sub :: IR Eval
sub = NativeFunc subImpl

-- Subtraction function implementation
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Sub.subImpl exactly
subImpl :: IR Eval -> Eval (IR Eval)
subImpl left = pure $ NativeFunc (subFrom left)

subFrom :: IR Eval -> IR Eval -> Eval (IR Eval)
subFrom left right = case (left, right) of
    (Integer a, Integer b) -> pure $ Integer (a - b)
    (Integer a, Float b) -> pure $ Float (fromIntegral a - b)
    (Float a, Integer b) -> pure $ Float (a - fromIntegral b)
    (Float a, Float b) -> pure $ Float (a - b)
    _ -> throwError $ wrongArgumentType ["number"]
