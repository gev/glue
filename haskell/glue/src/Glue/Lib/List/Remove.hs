module Glue.Lib.List.Remove where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

remove :: IR Eval
remove = NativeFunc removeImpl

removeImpl :: [IR Eval] -> Eval (IR Eval)
removeImpl [itemIR, listIR] = do
    item <- eval itemIR
    list <- eval listIR
    case list of
        List xs -> do
            let filtered = filter (/= item) xs
            pure $ List filtered
        _ -> throwError $ wrongArgumentType ["list"]
removeImpl _ = throwError wrongNumberOfArguments
