module Glue.Lib.Bool.Fallback where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongNumberOfArguments)
import Glue.IR (IR (..), exists)

fallback :: IR Eval
fallback = Special fallbackImpl

fallbackImpl :: [IR Eval] -> Eval (IR Eval)
fallbackImpl [a, b] = do
    a' <- eval a
    if exists a'
        then pure a'
        else eval b
fallbackImpl _ = throwError wrongNumberOfArguments
