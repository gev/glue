module Glue.Lib.Math.Power.Exp where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

exp :: [IR Eval] -> Eval (IR Eval)
exp [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.exp (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
exp _ = throwError WrongNumberOfArguments
