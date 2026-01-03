module Glue.Lib.Bool.Eq where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

eq :: [IR Eval] -> Eval (IR Eval)
eq [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    pure $ if va == vb then Symbol "true" else Symbol "false"
eq _ = throwError $ WrongArgumentType ["arg", "arg"]
