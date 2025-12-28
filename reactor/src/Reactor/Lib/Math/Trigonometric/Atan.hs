module Reactor.Lib.Math.Trigonometric.Atan where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

atan :: [IR Eval] -> Eval (IR Eval)
atan [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.atan (toRealFloat n)))
        _ -> throwError AtanExpectedOneNumber
atan _ = throwError WrongNumberOfArguments
