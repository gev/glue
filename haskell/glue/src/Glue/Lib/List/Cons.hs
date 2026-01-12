module Glue.Lib.List.Cons where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

cons :: [IR Eval] -> Eval (IR Eval)
cons [headArg, tailArg] = do
    headVal <- eval headArg
    tailVal <- eval tailArg
    case tailVal of
        List xs -> pure $ List (headVal : xs)
        _ -> throwError $ wrongArgumentType ["list"]
cons _ = throwError wrongNumberOfArguments
