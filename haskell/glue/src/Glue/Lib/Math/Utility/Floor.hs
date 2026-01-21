module Glue.Lib.Math.Utility.Floor where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Floor function
-- Mirrors Haskell Glue.Lib.Math.Utility.Floor.floor exactly
floor :: IR Eval
floor = NativeFunc floorImpl

-- Floor function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Floor.floorImpl exactly
floorImpl :: IR Eval -> Eval (IR Eval)
floorImpl arg = case arg of
    Integer n -> pure $ Integer n
    Float n -> pure $ Integer (Prelude.floor n)
    _ -> throwError $ wrongArgumentType ["number"]
