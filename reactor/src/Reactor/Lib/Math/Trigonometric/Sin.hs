module Reactor.Lib.Math.Trigonometric.Sin where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

sin :: [IR Eval] -> Eval (IR Eval)
sin [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits (Prelude.sin (toRealFloat n)))
        _ -> throwError SinExpectedOneNumber
sin _ = throwError WrongNumberOfArguments
