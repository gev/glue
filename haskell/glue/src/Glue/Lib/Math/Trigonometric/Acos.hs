module Glue.Lib.Math.Trigonometric.Acos where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

acos :: [IR Eval] -> Eval (IR Eval)
acos [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.acos (fromIntegral n))
        Float n -> pure $ Float (Prelude.acos n)
        _ -> throwError $ wrongArgumentType ["number"]
acos _ = throwError wrongNumberOfArguments
