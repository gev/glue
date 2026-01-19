module Glue.Lib.List.Drop where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

drop :: IR Eval
drop = NativeFunc dropImpl

dropImpl :: IR Eval -> Eval (IR Eval)
dropImpl countIR = pure $ NativeFunc (dropFrom countIR)

dropFrom :: IR Eval -> IR Eval -> Eval (IR Eval)
dropFrom countIR listIR = do
    count <- eval countIR
    list <- eval listIR
    case (count, list) of
        (Integer n, List xs) -> do
            if n < 0
                then throwError $ wrongArgumentType ["non-negative integer"]
                else pure $ List (Prelude.drop (fromIntegral n) xs)
        _ -> throwError $ wrongArgumentType ["number", "list"]
