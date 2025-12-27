module Reactor.Lib.Math.Utility.Abs where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (EvalError (..))
import Reactor.IR (IR (..))

abs :: [IR Eval] -> Eval (IR Eval)
abs [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits (Prelude.abs (toRealFloat n)))
        _ -> throwError AbsExpectedOneNumber
abs _ = throwError WrongNumberOfArguments
