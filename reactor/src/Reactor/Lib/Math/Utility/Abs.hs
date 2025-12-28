module Reactor.Lib.Math.Utility.Abs where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))
import Reactor.Lib.Math.Error (MathError (..))

abs :: [IR Eval] -> Eval (IR Eval)
abs [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.abs (toRealFloat n)))
        _ -> throwError AbsExpectedOneNumber
abs _ = throwError WrongNumberOfArguments
