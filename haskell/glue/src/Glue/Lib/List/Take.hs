module Glue.Lib.List.Take where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

take :: [IR Eval] -> Eval (IR Eval)
take [countIR, listIR] = do
    count <- eval countIR
    list <- eval listIR
    case (count, list) of
        (Integer n, List xs) -> do
            if n < 0
                then throwError $ wrongArgumentType ["non-negative integer"]
                else pure $ List (Prelude.take (fromIntegral n) xs)
        _ -> throwError $ wrongArgumentType ["number", "list"]
take _ = throwError wrongNumberOfArguments
