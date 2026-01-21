module Glue.Lib.Math.Utility.Max where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

-- Maximum function
-- Mirrors Haskell Glue.Lib.Math.Utility.Max.max exactly
max :: IR Eval
max = NativeFunc maxImpl

-- Maximum function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Max.maxImpl exactly
maxImpl :: IR Eval -> Eval (IR Eval)
maxImpl arg1 = pure $ NativeFunc (maxWith arg1)

maxWith :: IR Eval -> IR Eval -> Eval (IR Eval)
maxWith arg1 arg2 = case (arg1, arg2) of
    (Integer n1, Integer n2) -> pure $ Integer (Prelude.max n1 n2)
    (Float n1, Float n2) -> pure $ Float (Prelude.max n1 n2)
    (Integer n1, Float n2) -> pure $ Float (Prelude.max (fromIntegral n1) n2)
    (Float n1, Integer n2) -> pure $ Float (Prelude.max n1 (fromIntegral n2))
    _ -> throwError $ wrongArgumentType ["number", "number"]
