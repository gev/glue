module Glue.Lib.List.Cdr where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

cdr :: [IR Eval] -> Eval (IR Eval)
cdr [arg] = do
    val <- eval arg
    case val of
        List (_ : xs) -> pure $ List xs
        List [] -> throwError $ wrongArgumentType ["non-empty list"]
        _ -> throwError $ wrongArgumentType ["list"]
cdr _ = throwError wrongNumberOfArguments
