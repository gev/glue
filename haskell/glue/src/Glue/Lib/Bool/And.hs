module Glue.Lib.Bool.And where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongNumberOfArguments)
import Glue.IR (IR (..), isFalsy, isTruthy)

and_ :: IR Eval
and_ = Special andImpl

andImpl :: [IR Eval] -> Eval (IR Eval)
andImpl [a, b] = do
    a' <- eval a
    if isFalsy a'
        then pure (Bool False)
        else Bool . isTruthy <$> eval b
andImpl _ = throwError wrongNumberOfArguments
