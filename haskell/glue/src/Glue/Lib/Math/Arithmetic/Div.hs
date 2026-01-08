module Glue.Lib.Math.Arithmetic.Div where

import Glue.Eval.Exception

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

div :: [IR Eval] -> Eval (IR Eval)
div [left, right] = do
    l <- evalRequired left
    r <- evalRequired right
    case (l, r) of
        (Integer a, Integer b) -> do
            if b == 0
                then throwError divByZero
                else pure $ Float (fromIntegral a / fromIntegral b)
        (Integer a, Float b) -> do
            if b == 0
                then throwError divByZero
                else pure $ Float (fromIntegral a / b)
        (Float a, Integer b) -> do
            if b == 0
                then throwError divByZero
                else pure $ Float (a / fromIntegral b)
        (Float a, Float b) -> do
            if b == 0
                then throwError divByZero
                else pure $ Float (a / b)
        _ -> throwError $ wrongArgumentType ["number"]
div _ = throwError wrongNumberOfArguments
