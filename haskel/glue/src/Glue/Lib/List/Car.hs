module Glue.Lib.List.Car where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

car :: [IR Eval] -> Eval (IR Eval)
car [arg] = do
    val <- evalRequired arg
    case val of
        List (x : _) -> pure x
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        _ -> throwError $ WrongArgumentType ["list"]
car _ = throwError WrongNumberOfArguments
