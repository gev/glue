module Glue.Lib.List.Append where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

append :: [IR Eval] -> Eval (IR Eval)
append args = do
    case args of
        [list1, list2] -> do
            val1 <- evalRequired list1
            val2 <- evalRequired list2
            case (val1, val2) of
                (List xs, List ys) -> pure $ List (xs ++ ys)
                _ -> throwError $ wrongArgumentType ["list", "list"]
        _ -> throwError wrongNumberOfArguments
