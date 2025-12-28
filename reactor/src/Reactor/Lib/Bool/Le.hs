module Reactor.Lib.Bool.Le where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Bool.Error (BoolError (..))

le :: [IR Eval] -> Eval (IR Eval)
le [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure $ if na <= nb then Symbol "true" else Symbol "false"
        _ -> throwError LeExpectedNumbers
le _ = throwError LeExpectedTwoArgs
