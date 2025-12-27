module Reactor.Lib.Bool.Ge where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

ge :: [IR Eval] -> Eval (IR Eval)
ge [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure $ if na >= nb then Symbol "true" else Symbol "false"
        _ -> throwError GeExpectedNumbers
ge _ = throwError GeExpectedTwoArguments
