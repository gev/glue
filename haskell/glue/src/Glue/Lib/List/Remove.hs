module Glue.Lib.List.Remove where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

remove :: [IR Eval] -> Eval (IR Eval)
remove [itemIR, listIR] = do
    item <- eval itemIR
    list <- eval listIR
    case list of
        List xs -> do
            let filtered = filter (/= item) xs
            pure $ List filtered
        _ -> throwError $ wrongArgumentType ["list"]
remove _ = throwError wrongNumberOfArguments
