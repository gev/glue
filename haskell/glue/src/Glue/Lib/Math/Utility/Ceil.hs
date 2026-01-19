module Glue.Lib.Math.Utility.Ceil where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Ceiling function
-- Mirrors Haskell Glue.Lib.Math.Utility.Ceil.ceil exactly
ceil :: IR Eval
ceil = NativeFunc ceilImpl

-- Ceiling function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Ceil.ceilImpl exactly
ceilImpl :: [IR Eval] -> Eval (IR Eval)
ceilImpl [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.ceiling n)
        _ -> throwError $ wrongArgumentType ["number"]
ceilImpl _ = throwError wrongNumberOfArguments
