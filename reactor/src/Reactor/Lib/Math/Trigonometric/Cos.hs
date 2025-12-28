module Reactor.Lib.Math.Trigonometric.Cos where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

cos :: [IR Eval] -> Eval (IR Eval)
cos [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.cos (toRealFloat n)))
        _ -> throwError CosExpectedOneNumber
cos _ = throwError WrongNumberOfArguments
