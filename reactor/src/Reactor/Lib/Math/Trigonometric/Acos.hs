module Reactor.Lib.Math.Trigonometric.Acos where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

acos :: [IR Eval] -> Eval (IR Eval)
acos [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.acos (toRealFloat n)))
        _ -> throwError AcosExpectedOneNumber
acos _ = throwError WrongNumberOfArguments
