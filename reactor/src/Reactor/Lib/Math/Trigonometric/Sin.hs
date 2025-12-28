module Reactor.Lib.Math.Trigonometric.Sin where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

sin :: [IR Eval] -> Eval (IR Eval)
sin [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.sin (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
sin _ = throwError $ WrongArgumentType ["number"]
