module Glue.Lib.Math.Trigonometric.Asin where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

asin :: [IR Eval] -> Eval (IR Eval)
asin [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.asin (fromIntegral n))
        Float n -> pure $ Float (Prelude.asin n)
        _ -> throwError $ wrongArgumentType ["number"]
asin _ = throwError wrongNumberOfArguments
