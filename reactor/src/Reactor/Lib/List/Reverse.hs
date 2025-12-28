module Reactor.Lib.List.Reverse where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

reverse :: [IR Eval] -> Eval (IR Eval)
reverse [arg] = do
    val <- evalRequired arg
    case val of
        List xs -> pure $ List (Prelude.reverse xs)
        _ -> throwError $ WrongArgumentType ["list"]
reverse _ = throwError WrongNumberOfArguments
