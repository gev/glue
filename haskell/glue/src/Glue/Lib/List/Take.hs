module Glue.Lib.List.Take where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

take :: [IR Eval] -> Eval (IR Eval)
take [countIR, listIR] = do
    count <- evalRequired countIR
    list <- evalRequired listIR
    case (count, list) of
        (Number n, List xs) -> do
            if n < 0
                then throwError $ wrongArgumentType ["non-negative integer"]
                else pure $ List (Prelude.take (floor n) xs)
        _ -> throwError $ wrongArgumentType ["number", "list"]
take _ = throwError wrongNumberOfArguments
