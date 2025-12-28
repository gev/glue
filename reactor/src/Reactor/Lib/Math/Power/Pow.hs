module Reactor.Lib.Math.Power.Pow where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

pow :: [IR Eval] -> Eval (IR Eval)
pow [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> pure $ Number (fromFloatDigits @Double (toRealFloat n1 ** toRealFloat n2))
        _ -> throwError $ WrongArgumentType "pow" ["number", "number"]
pow _ = throwError WrongNumberOfArguments
