module Reactor.Lib.Math.Trigonometric.Asin where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

asin :: [IR Eval] -> Eval (IR Eval)
asin [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits (Prelude.asin (toRealFloat n)))
        _ -> throwError AsinExpectedOneNumber
asin _ = throwError WrongNumberOfArguments
