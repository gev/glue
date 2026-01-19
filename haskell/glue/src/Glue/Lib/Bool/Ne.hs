module Glue.Lib.Bool.Ne where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

ne :: IR Eval
ne = NativeFunc neImpl

neImpl :: [IR Eval] -> Eval (IR Eval)
neImpl [a, b] = do
    va <- eval a
    vb <- eval b
    pure . Bool $ va /= vb
neImpl _ = throwError $ wrongArgumentType ["arg", "arg"]
