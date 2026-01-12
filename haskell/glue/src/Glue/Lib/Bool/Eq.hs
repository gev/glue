module Glue.Lib.Bool.Eq where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

eq :: [IR Eval] -> Eval (IR Eval)
eq [a, b] = do
    va <- eval a
    vb <- eval b
    pure . Bool $ va == vb
eq _ = throwError $ wrongArgumentType ["arg", "arg"]
