module Glue.Lib.List.Take where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

take :: IR Eval
take = NativeFunc takeImpl

takeImpl :: IR Eval -> Eval (IR Eval)
takeImpl countIR = pure $ NativeFunc (takeFrom countIR)

takeFrom :: IR Eval -> IR Eval -> Eval (IR Eval)
takeFrom countIR listIR = do
    count <- eval countIR
    list <- eval listIR
    case (count, list) of
        (Integer n, List xs) -> do
            if n < 0
                then throwError $ wrongArgumentType ["non-negative integer"]
                else pure $ List (Prelude.take (fromIntegral n) xs)
        _ -> throwError $ wrongArgumentType ["number", "list"]
