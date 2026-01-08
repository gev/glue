module Glue.Lib.Math.Arithmetic.Sub where

import Glue.Eval.Exception

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

sub :: [IR Eval] -> Eval (IR Eval)
sub [left, right] = do
    l <- evalRequired left
    r <- evalRequired right
    case (l, r) of
        (Integer a, Integer b) -> pure $ Integer (a - b)
        (Integer a, Float b) -> pure $ Float (fromIntegral a - b)
        (Float a, Integer b) -> pure $ Float (a - fromIntegral b)
        (Float a, Float b) -> pure $ Float (a - b)
        _ -> throwError $ wrongArgumentType ["number"]
sub _ = throwError wrongNumberOfArguments
