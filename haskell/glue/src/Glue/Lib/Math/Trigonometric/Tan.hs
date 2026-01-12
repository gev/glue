module Glue.Lib.Math.Trigonometric.Tan where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

tan :: [IR Eval] -> Eval (IR Eval)
tan [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.tan (fromIntegral n))
        Float n -> pure $ Float (Prelude.tan n)
        _ -> throwError $ wrongArgumentType ["number"]
tan _ = throwError wrongNumberOfArguments
