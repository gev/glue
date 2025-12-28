module Reactor.Lib.Math.Logarithmic.Log where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (Error, GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

log :: [IR Eval] -> Eval (IR Eval)
log [arg, base] = do
    va <- evalRequired arg
    vb <- evalRequired base
    case (va, vb) of
        (Number n, Number b) -> do
            let realVal = toRealFloat @Double n
            let realBase = toRealFloat @Double b
            if realVal <= 0 || realBase <= 0 || realBase == 1
                then throwError LogExpectedPositiveNumber
                else pure $ Number (fromFloatDigits (Prelude.logBase realBase realVal))
        _ -> throwError LogExpectedPositiveNumber
log _ = throwError WrongNumberOfArguments
