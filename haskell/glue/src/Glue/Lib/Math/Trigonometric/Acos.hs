module Glue.Lib.Math.Trigonometric.Acos where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

acos :: [IR Eval] -> Eval (IR Eval)
acos [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.acos (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
acos _ = throwError WrongNumberOfArguments
