module Glue.Lib.Math.Utility.Round where

import Data.Scientific (toRealFloat)
import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

round :: [IR Eval] -> Eval (IR Eval)
round [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> pure $ Number (fromIntegral @Int (Prelude.round @Double (toRealFloat n)))
        _ -> throwError $ WrongArgumentType ["number"]
round _ = throwError WrongNumberOfArguments
