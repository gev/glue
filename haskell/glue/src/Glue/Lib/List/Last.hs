module Glue.Lib.List.Last where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

last :: [IR Eval] -> Eval (IR Eval)
last [arg] = do
    val <- evalRequired arg
    case val of
        List [] -> throwError $ WrongArgumentType ["non-empty list"]
        List xs -> pure $ Prelude.last xs
        _ -> throwError $ WrongArgumentType ["list"]
last _ = throwError WrongNumberOfArguments
