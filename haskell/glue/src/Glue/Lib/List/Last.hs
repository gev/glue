module Glue.Lib.List.Last where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

last :: [IR Eval] -> Eval (IR Eval)
last [arg] = do
    val <- eval arg
    case val of
        List [] -> throwError $ wrongArgumentType ["non-empty list"]
        List xs -> pure $ Prelude.last xs
        _ -> throwError $ wrongArgumentType ["list"]
last _ = throwError wrongNumberOfArguments
