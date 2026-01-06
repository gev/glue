module Glue.Lib.List.Cdr where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

cdr :: [IR Eval] -> Eval (IR Eval)
cdr [arg] = do
    val <- evalRequired arg
    case val of
        List (_ : xs) -> pure $ List xs
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        _ -> throwError $ WrongArgumentType ["list"]
cdr _ = throwError WrongNumberOfArguments
