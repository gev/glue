module Glue.Lib.Math.Arithmetic.Add where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Addition function
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Add.add exactly
add :: IR Eval
add = NativeFunc addImpl

-- Addition function implementation
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Add.addImpl exactly
addImpl :: IR Eval -> Eval (IR Eval)
addImpl left = pure $ NativeFunc (addTo left)

addTo :: IR Eval -> IR Eval -> Eval (IR Eval)
addTo left right = case (left, right) of
    (Integer a, Integer b) -> pure $ Integer (a + b)
    (Integer a, Float b) -> pure $ Float (fromIntegral a + b)
    (Float a, Integer b) -> pure $ Float (a + fromIntegral b)
    (Float a, Float b) -> pure $ Float (a + b)
    _ -> throwError $ wrongArgumentType ["number"]
