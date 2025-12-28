module Reactor.Lib.Bool.Not where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (Error)
import Reactor.IR (IR (..))
import Reactor.Lib.Bool.Error (BoolError (..))

not_ :: [IR Eval] -> Eval (IR Eval)
not_ [arg] = do
    val <- evalRequired arg
    case val of
        Symbol "false" -> pure $ Symbol "true"
        _ -> pure $ Symbol "false"
not_ _ = throwError NotExpectedOneArg
