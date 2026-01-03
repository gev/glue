module Glue.Lib.Math.Utility.Min where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

min :: [IR Eval] -> Eval (IR Eval)
min [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> pure $ Number (Prelude.min n1 n2)
        _ -> throwError $ WrongArgumentType ["number", "number"]
min _ = throwError WrongNumberOfArguments
