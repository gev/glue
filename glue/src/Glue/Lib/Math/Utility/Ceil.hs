module Glue.Lib.Math.Utility.Ceil where

import Data.Scientific (toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

ceil :: [IR Eval] -> Eval (IR Eval)
ceil [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral @Int (Prelude.ceiling @Double (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
ceil _ = throwError WrongNumberOfArguments
