module Reactor.Lib.Math.Logarithmic.Lg where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

lg :: [IR Eval] -> Eval (IR Eval)
lg [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal <= 0
                then throwError LogExpectedPositiveNumber
                else pure $ Number (fromFloatDigits (Prelude.logBase 10 realVal))
        _ -> throwError LogExpectedPositiveNumber
lg _ = throwError WrongNumberOfArguments
