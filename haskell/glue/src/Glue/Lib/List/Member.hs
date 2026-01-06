module Glue.Lib.List.Member where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

member :: [IR Eval] -> Eval (IR Eval)
member [itemIR, listIR] = do
    item <- evalRequired itemIR
    list <- evalRequired listIR
    case list of
        List xs -> pure . Bool $ item `elem` xs
        _ -> throwError $ wrongArgumentType ["list"]
member _ = throwError wrongNumberOfArguments
