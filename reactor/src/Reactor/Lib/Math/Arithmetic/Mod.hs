module Reactor.Lib.Math.Arithmetic.Mod where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

mod :: [IR Eval] -> Eval (IR Eval)
mod [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> do
            if n2 == 0
                then throwError ModByZero
                else pure $ Number (fromIntegral @Int (truncate n1 `Prelude.mod` truncate n2))
        _ -> throwError ModExpectedTwoNumbers
mod _ = throwError ModExpectedTwoNumbers
