module Glue.Lib.List.Car where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

car :: [IR Eval] -> Eval (IR Eval)
car [arg] = do
    val <- eval arg
    case val of
        List (x : _) -> pure x
        List [] -> throwError $ wrongArgumentType ["non-empty list"]
        _ -> throwError $ wrongArgumentType ["list"]
car _ = throwError wrongNumberOfArguments
