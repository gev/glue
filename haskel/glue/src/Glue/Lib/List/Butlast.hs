module Glue.Lib.List.Butlast where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

butlast :: [IR Eval] -> Eval (IR Eval)
butlast [arg] = do
    val <- evalRequired arg
    case val of
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        List [_] -> pure $ List []
        List xs -> pure $ List (init xs)
        _ -> throwError $ WrongArgumentType ["list"]
butlast _ = throwError WrongNumberOfArguments
