module Glue.Lib.Math.Utility.Ceil where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

ceil :: [IR Eval] -> Eval (IR Eval)
ceil [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.ceiling n)
        _ -> throwError $ wrongArgumentType ["number"]
ceil _ = throwError wrongNumberOfArguments
