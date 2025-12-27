module Reactor.Lib.Math.Trigonometric.Tan where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

tan :: [IR Eval] -> Eval (IR Eval)
tan [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits (Prelude.tan (toRealFloat n)))
        _ -> throwError TanExpectedOneNumber
tan _ = throwError WrongNumberOfArguments
