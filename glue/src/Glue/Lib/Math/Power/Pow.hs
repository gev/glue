module Glue.Lib.Math.Power.Pow where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

pow :: [IR Eval] -> Eval (IR Eval)
pow [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> pure $ Number (fromFloatDigits @Double (toRealFloat n1 ** toRealFloat n2))
        _ -> throwError $ WrongArgumentType ["number", "number"]
pow _ = throwError WrongNumberOfArguments
