module Reactor.Lib.Math.Power.Exp where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

exp :: [IR Eval] -> Eval (IR Eval)
exp [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.exp (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
exp _ = throwError WrongNumberOfArguments
