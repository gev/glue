module Glue.Lib.Math.Utility.Ceil where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Ceiling function
-- Mirrors Haskell Glue.Lib.Math.Utility.Ceil.ceil exactly
ceil :: IR Eval
ceil = NativeFunc ceilImpl

-- Ceiling function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Ceil.ceilImpl exactly
ceilImpl :: IR Eval -> Eval (IR Eval)
ceilImpl arg = case arg of
    Integer n -> pure $ Integer n
    Float n -> pure $ Integer (Prelude.ceiling n)
    _ -> throwError $ wrongArgumentType ["number"]
