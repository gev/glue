module Glue.Lib.Math.Utility.Trunc where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Truncate function
-- Mirrors Haskell Glue.Lib.Math.Utility.Trunc.trunc exactly
trunc :: IR Eval
trunc = NativeFunc truncImpl

-- Truncate function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Trunc.truncImpl exactly
truncImpl :: IR Eval -> Eval (IR Eval)
truncImpl arg = case arg of
    Integer n -> pure $ Integer n
    Float n -> pure $ Integer (Prelude.truncate n)
    _ -> throwError $ wrongArgumentType ["number"]
