module Reactor.Lib.Math.Trigonometric.Tan where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

tan :: [IR Eval] -> Eval (IR Eval)
tan [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.tan (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
tan _ = throwError WrongNumberOfArguments
