module Glue.Lib.Math.Utility.Abs where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

abs :: [IR Eval] -> Eval (IR Eval)
abs [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer (Prelude.abs n)
        Float n -> pure $ Float (Prelude.abs n)
        _ -> throwError $ wrongArgumentType ["number"]
abs _ = throwError wrongNumberOfArguments
