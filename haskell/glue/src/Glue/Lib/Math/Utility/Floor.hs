module Glue.Lib.Math.Utility.Floor where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

floor :: [IR Eval] -> Eval (IR Eval)
floor [arg] = do
    va <- eval arg
    case va of
        Integer n -> pure $ Integer n
        Float n -> pure $ Integer (Prelude.floor n)
        _ -> throwError $ wrongArgumentType ["number"]
floor _ = throwError wrongNumberOfArguments
