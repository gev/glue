module Glue.Lib.Math.Utility.Round where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

round :: [IR Eval] -> Eval (IR Eval)
round [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.round n)
        _ -> throwError $ wrongArgumentType ["number"]
round _ = throwError wrongNumberOfArguments
