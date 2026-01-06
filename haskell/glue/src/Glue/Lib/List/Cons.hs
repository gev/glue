module Glue.Lib.List.Cons where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

cons :: [IR Eval] -> Eval (IR Eval)
cons [headArg, tailArg] = do
    headVal <- evalRequired headArg
    tailVal <- evalRequired tailArg
    case tailVal of
        List xs -> pure $ List (headVal : xs)
        _ -> throwError $ WrongArgumentType ["list"]
cons _ = throwError WrongNumberOfArguments
