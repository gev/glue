module Glue.Lib.Bool.Or where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongNumberOfArguments)
import Glue.IR (IR (..), isTruthy)

or_ :: IR Eval
or_ = Special orImpl

orImpl :: [IR Eval] -> Eval (IR Eval)
orImpl [a, b] = do
    a' <- eval a
    if isTruthy a'
        then pure (Bool True)
        else Bool . isTruthy <$> eval b
orImpl _ = throwError wrongNumberOfArguments
