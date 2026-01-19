module Glue.Lib.List.Append where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

append :: IR Eval
append = NativeFunc appendImpl

appendImpl :: [IR Eval] -> Eval (IR Eval)
appendImpl args = do
    case args of
        [list1, list2] -> do
            val1 <- eval list1
            val2 <- eval list2
            case (val1, val2) of
                (List xs, List ys) -> pure $ List (xs ++ ys)
                _ -> throwError $ wrongArgumentType ["list", "list"]
        _ -> throwError wrongNumberOfArguments
