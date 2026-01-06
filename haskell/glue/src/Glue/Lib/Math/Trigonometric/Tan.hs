module Glue.Lib.Math.Trigonometric.Tan where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

tan :: [IR Eval] -> Eval (IR Eval)
tan [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.tan (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
tan _ = throwError WrongNumberOfArguments
