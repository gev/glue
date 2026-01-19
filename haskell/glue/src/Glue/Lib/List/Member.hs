module Glue.Lib.List.Member where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

member :: IR Eval
member = NativeFunc memberImpl

memberImpl :: [IR Eval] -> Eval (IR Eval)
memberImpl [itemIR, listIR] = do
    item <- eval itemIR
    list <- eval listIR
    case list of
        List xs -> pure . Bool $ item `elem` xs
        _ -> throwError $ wrongArgumentType ["list"]
memberImpl _ = throwError wrongNumberOfArguments
