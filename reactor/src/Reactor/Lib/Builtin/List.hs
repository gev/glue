module Reactor.Lib.Builtin.List where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

list :: [IR Eval] -> Eval (IR Eval)
list args = pure (List args)

car :: [IR Eval] -> Eval (IR Eval)
car [arg] = do
    val <- evalRequired arg
    case val of
        List (x : _) -> pure x
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        _ -> throwError $ WrongArgumentType ["list"]
car _ = throwError WrongNumberOfArguments
