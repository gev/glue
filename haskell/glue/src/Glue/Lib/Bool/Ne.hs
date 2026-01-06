module Glue.Lib.Bool.Ne where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

ne :: [IR Eval] -> Eval (IR Eval)
ne [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    pure . Bool $ va /= vb
ne _ = throwError $ wrongArgumentType ["arg", "arg"]
