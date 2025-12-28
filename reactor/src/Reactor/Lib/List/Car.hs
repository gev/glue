module Reactor.Lib.List.Car where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

car :: [IR Eval] -> Eval (IR Eval)
car [arg] = do
    val <- evalRequired arg
    case val of
        List (x : _) -> pure x
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        _ -> throwError $ WrongArgumentType ["list"]
car _ = throwError WrongNumberOfArguments
