module Glue.Lib.Math.Arithmetic.Mod where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

mod :: [IR Eval] -> Eval (IR Eval)
mod [arg1, arg2] = do
    va1 <- evalRequired arg1
    va2 <- evalRequired arg2
    case (va1, va2) of
        (Number n1, Number n2) -> do
            if n2 == 0
                then throwError DivByZero
                else pure $ Number (fromIntegral @Int (truncate n1 `Prelude.mod` truncate n2))
        _ -> throwError $ WrongArgumentType ["number", "number"]
mod _ = throwError WrongNumberOfArguments
