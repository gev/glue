module Reactor.Lib.Math.Utility.Min where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

min :: [IR Eval] -> Eval (IR Eval)
min [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> pure $ Number (Prelude.min n1 n2)
        _ -> throwError MinExpectedTwoNumbers
min _ = throwError WrongNumberOfArguments
