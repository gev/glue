module Reactor.Lib.Math.Power.Sqrt where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (Error, GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

sqrt :: [IR Eval] -> Eval (IR Eval)
sqrt [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal < 0
                then throwError SqrtExpectedOneNumber
                else pure $ Number (fromFloatDigits (Prelude.sqrt realVal))
        _ -> throwError SqrtExpectedOneNumber
sqrt _ = throwError WrongNumberOfArguments
