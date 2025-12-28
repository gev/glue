module Reactor.Lib.List.Cdr where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

cdr :: [IR Eval] -> Eval (IR Eval)
cdr [arg] = do
    val <- evalRequired arg
    case val of
        List (_ : xs) -> pure $ List xs
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        _ -> throwError $ WrongArgumentType ["list"]
cdr _ = throwError WrongNumberOfArguments
