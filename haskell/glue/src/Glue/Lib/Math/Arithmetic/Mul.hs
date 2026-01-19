module Glue.Lib.Math.Arithmetic.Mul where

import Glue.Eval.Exception

import Glue.Eval (Eval, eval, throwError)
import Glue.IR (IR (..))

-- Multiplication function
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Mul.mul exactly
mul :: IR Eval
mul = NativeFunc mulImpl

-- Multiplication function implementation
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Mul.mulImpl exactly
mulImpl :: [IR Eval] -> Eval (IR Eval)
mulImpl [left, right] = do
    l <- eval left
    r <- eval right
    case (l, r) of
        (Integer a, Integer b) -> pure $ Integer (a * b)
        (Integer a, Float b) -> pure $ Float (fromIntegral a * b)
        (Float a, Integer b) -> pure $ Float (a * fromIntegral b)
        (Float a, Float b) -> pure $ Float (a * b)
        _ -> throwError $ wrongArgumentType ["number"]
mulImpl _ = throwError wrongNumberOfArguments
