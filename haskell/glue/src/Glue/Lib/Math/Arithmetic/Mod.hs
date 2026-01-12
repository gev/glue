module Glue.Lib.Math.Arithmetic.Mod where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

mod :: [IR Eval] -> Eval (IR Eval)
mod [arg1, arg2] = do
    va1 <- eval arg1
    va2 <- eval arg2
    case (va1, va2) of
        (Integer n1, Integer n2) -> do
            if n2 == 0
                then throwError divByZero
                else pure $ Integer (n1 `Prelude.mod` n2)
        (Float n1, Float n2) -> do
            if n2 == 0
                then throwError divByZero
                else pure $ Float (fromIntegral @Int (truncate n1 `Prelude.mod` truncate n2))
        (Integer n1, Float n2) -> do
            if n2 == 0
                then throwError divByZero
                else pure $ Float (fromIntegral @Int (n1 `Prelude.mod` truncate n2))
        (Float n1, Integer n2) -> do
            if n2 == 0
                then throwError divByZero
                else pure $ Float (fromIntegral @Int (truncate n1 `Prelude.mod` n2))
        _ -> throwError $ wrongArgumentType ["number", "number"]
mod _ = throwError wrongNumberOfArguments
