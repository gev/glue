module Glue.Lib.Math.Arithmetic.Mod where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (divByZero, wrongArgumentType)
import Glue.IR (IR (..))

-- Modulo function
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Mod.mod exactly
mod :: IR Eval
mod = NativeFunc modImpl

-- Modulo function implementation
-- Mirrors Haskell Glue.Lib.Math.Arithmetic.Mod.modImpl exactly
modImpl :: IR Eval -> Eval (IR Eval)
modImpl arg1 = pure $ NativeFunc (modBy arg1)

modBy :: IR Eval -> IR Eval -> Eval (IR Eval)
modBy arg1 arg2 = case (arg1, arg2) of
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
