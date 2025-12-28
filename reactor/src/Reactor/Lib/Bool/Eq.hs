module Reactor.Lib.Bool.Eq where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Bool.Error (BoolError (..))

eq :: [IR Eval] -> Eval (IR Eval)
eq [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    pure $ if va == vb then Symbol "true" else Symbol "false"
eq _ = throwError EqExpectedTwoArgs
