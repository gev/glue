module Reactor.Lib.List.Butlast where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

butlast :: [IR Eval] -> Eval (IR Eval)
butlast [arg] = do
    val <- evalRequired arg
    case val of
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        List [_] -> pure $ List []
        List xs -> pure $ List (init xs)
        _ -> throwError $ WrongArgumentType ["list"]
butlast _ = throwError WrongNumberOfArguments
