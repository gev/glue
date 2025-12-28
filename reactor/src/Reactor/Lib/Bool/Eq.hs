module Reactor.Lib.Bool.Eq where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

eq :: [IR Eval] -> Eval (IR Eval)
eq [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    pure $ if va == vb then Symbol "true" else Symbol "false"
eq _ = throwError $ WrongArgumentType ["arg", "arg"]
