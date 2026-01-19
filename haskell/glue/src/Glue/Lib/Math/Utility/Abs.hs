module Glue.Lib.Math.Utility.Abs where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

-- Absolute value function
-- Mirrors Haskell Glue.Lib.Math.Utility.Abs.abs exactly
abs :: IR Eval
abs = NativeFunc absImpl

-- Absolute value function implementation
-- Mirrors Haskell Glue.Lib.Math.Utility.Abs.absImpl exactly
absImpl :: IR Eval -> Eval (IR Eval)
absImpl arg = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer (Prelude.abs n)
        Float n -> pure $ Float (Prelude.abs n)
        _ -> throwError $ wrongArgumentType ["number"]
