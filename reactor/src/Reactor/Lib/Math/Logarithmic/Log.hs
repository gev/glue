module Reactor.Lib.Math.Logarithmic.Log where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

log :: [IR Eval] -> Eval (IR Eval)
log [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            let realVal = toRealFloat @Double n
            if realVal <= 0
                then throwError LogExpectedPositiveNumber
                else pure $ Number (fromFloatDigits (Prelude.log realVal))
        _ -> throwError LogExpectedPositiveNumber
log _ = throwError WrongNumberOfArguments
