module Glue.Lib.Math.Utility.Floor where

import Data.Scientific (toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

floor :: [IR Eval] -> Eval (IR Eval)
floor [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral @Int (Prelude.floor @Double (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
floor _ = throwError WrongNumberOfArguments
