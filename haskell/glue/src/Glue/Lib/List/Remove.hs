module Glue.Lib.List.Remove where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

remove :: [IR Eval] -> Eval (IR Eval)
remove [itemIR, listIR] = do
    item <- evalRequired itemIR
    list <- evalRequired listIR
    case list of
        List xs -> do
            let filtered = filter (/= item) xs
            pure $ List filtered
        _ -> throwError $ WrongArgumentType ["list"]
remove _ = throwError WrongNumberOfArguments
