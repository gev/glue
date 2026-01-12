module Glue.Lib.Math.Power.Exp where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

exp :: [IR Eval] -> Eval (IR Eval)
exp [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Float (Prelude.exp (fromIntegral n))
        Float n -> pure $ Float (Prelude.exp n)
        _ -> throwError $ wrongArgumentType ["number"]
exp _ = throwError wrongNumberOfArguments
