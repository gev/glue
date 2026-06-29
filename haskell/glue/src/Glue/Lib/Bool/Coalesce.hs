module Glue.Lib.Bool.Coalesce where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongNumberOfArguments)
import Glue.IR (IR (..), isExist)

coalesce :: IR Eval
coalesce = Special coalesceImpl

coalesceImpl :: [IR Eval] -> Eval (IR Eval)
coalesceImpl [a, b] = do
    a' <- eval a
    if isExist a'
        then pure a'
        else eval b
coalesceImpl _ = throwError wrongNumberOfArguments
