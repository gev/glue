module Glue.Lib.Math.Trigonometric.Cos where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

cos :: [IR Eval] -> Eval (IR Eval)
cos [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.cos (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
cos _ = throwError WrongNumberOfArguments
