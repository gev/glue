module Glue.Lib.Math.Utility.Abs where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

abs :: [IR Eval] -> Eval (IR Eval)
abs [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.abs (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
abs _ = throwError WrongNumberOfArguments
