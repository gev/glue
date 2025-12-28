module Reactor.Lib.Math.Trigonometric.Asin where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

asin :: [IR Eval] -> Eval (IR Eval)
asin [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.asin (toRealFloat n)))
        _ -> throwError $ WrongArgumentType "asin" ["number"]
asin _ = throwError WrongNumberOfArguments
