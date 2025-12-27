module Reactor.Lib.Bool.Gt where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

gt :: [IR Eval] -> Eval (IR Eval)
gt [a, b] = do
    va <- evalRequired a
    vb <- evalRequired b
    case (va, vb) of
        (Number na, Number nb) -> pure $ if na > nb then Symbol "true" else Symbol "false"
        _ -> throwError GtExpectedNumbers
gt _ = throwError GtExpectedTwoArguments
