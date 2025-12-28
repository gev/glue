module Reactor.Lib.Bool.Ne where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Bool.Error (BoolError (..))

ne :: [IR Eval] -> Eval (IR Eval)
ne [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    pure $ if va /= vb then Symbol "true" else Symbol "false"
ne _ = throwError NeExpectedTwoArgs
