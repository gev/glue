module Reactor.Lib.Math.Trigonometric.Cos where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

cos :: [IR Eval] -> Eval (IR Eval)
cos [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits (Prelude.cos (toRealFloat n)))
        _ -> throwError CosExpectedOneNumber
cos _ = throwError WrongNumberOfArguments
