module Reactor.Lib.List.Cons where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

cons :: [IR Eval] -> Eval (IR Eval)
cons [headArg, tailArg] = do
    headVal <- evalRequired headArg
    tailVal <- evalRequired tailArg
    case tailVal of
        List xs -> pure $ List (headVal : xs)
        _ -> throwError $ WrongArgumentType ["list"]
cons _ = throwError WrongNumberOfArguments
