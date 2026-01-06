module Glue.Lib.List.Butlast where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

butlast :: [IR Eval] -> Eval (IR Eval)
butlast [arg] = do
    val <- evalRequired arg
    case val of
        List [] -> throwError $ wrongArgumentType ["non-empty list"]
        List [_] -> pure $ List []
        List xs -> pure $ List (init xs)
        _ -> throwError $ wrongArgumentType ["list"]
butlast _ = throwError wrongNumberOfArguments
