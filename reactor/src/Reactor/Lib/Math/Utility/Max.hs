module Reactor.Lib.Math.Utility.Max where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

max :: [IR Eval] -> Eval (IR Eval)
max [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> pure $ Number (Prelude.max n1 n2)
        _ -> throwError MaxExpectedTwoNumbers
max _ = throwError WrongNumberOfArguments
