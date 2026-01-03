module Glue.Lib.Bool.Ne where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

ne :: [IR Eval] -> Eval (IR Eval)
ne [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    pure $ if va /= vb then Symbol "true" else Symbol "false"
ne _ = throwError $ WrongArgumentType ["arg", "arg"]
