module Reactor.Lib.Math.Trigonometric.Atan where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

atan :: [IR Eval] -> Eval (IR Eval)
atan [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.atan (toRealFloat n)))
        _ -> throwError AtanExpectedOneNumber
atan _ = throwError WrongNumberOfArguments
