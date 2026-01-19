module Glue.Lib.Math.Arithmetic.Div where

import Glue.Eval.Exception

import Glue.Eval (Eval, eval, throwError)
import Glue.IR (IR (..))

-- Division function
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.div exactly
div :: IR Eval
div = NativeFunc divImpl

-- Division function implementation
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Div.divImpl exactly
divImpl :: [IR Eval] -> Eval (IR Eval)
divImpl [left, right] = do
    l <- eval left
    r <- eval right
    case (l, r) of
        (Integer a, Integer b) -> pure $ Float (fromIntegral a / fromIntegral b)
        (Integer a, Float b) -> pure $ Float (fromIntegral a / b)
        (Float a, Integer b) -> pure $ Float (a / fromIntegral b)
        (Float a, Float b) -> pure $ Float (a / b)
        _ -> throwError $ wrongArgumentType ["number"]
divImpl _ = throwError wrongNumberOfArguments
