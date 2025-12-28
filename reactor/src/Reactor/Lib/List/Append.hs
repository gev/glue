module Reactor.Lib.List.Append where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

append :: [IR Eval] -> Eval (IR Eval)
append args = do
    case args of
        [list1, list2] -> do
            val1 <- evalRequired list1
            val2 <- evalRequired list2
            case (val1, val2) of
                (List xs, List ys) -> pure $ List (xs ++ ys)
                _ -> throwError $ WrongArgumentType ["list", "list"]
        _ -> throwError WrongNumberOfArguments
