module Glue.Lib.Math.Trigonometric.Sin where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

sin :: [IR Eval] -> Eval (IR Eval)
sin [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.sin (fromIntegral n))
        Float n -> pure $ Float (Prelude.sin n)
        _ -> throwError $ wrongArgumentType ["number"]
sin _ = throwError $ wrongArgumentType ["number"]
