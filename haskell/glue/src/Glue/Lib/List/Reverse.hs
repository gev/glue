module Glue.Lib.List.Reverse where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

reverse :: [IR Eval] -> Eval (IR Eval)
reverse [arg] = do
    val <- evalRequired arg
    case val of
        List xs -> pure $ List (Prelude.reverse xs)
        _ -> throwError $ WrongArgumentType ["list"]
reverse _ = throwError WrongNumberOfArguments
