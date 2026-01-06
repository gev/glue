module Glue.Lib.Math.Trigonometric.Asin where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

asin :: [IR Eval] -> Eval (IR Eval)
asin [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromFloatDigits @Double (Prelude.asin (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
asin _ = throwError WrongNumberOfArguments
