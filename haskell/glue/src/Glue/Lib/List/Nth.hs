module Glue.Lib.List.Nth where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

nth :: [IR Eval] -> Eval (IR Eval)
nth [indexIR, listIR] = do
    indexVal <- eval indexIR
    listVal <- eval listIR
    case (indexVal, listVal) of
        (Integer idx, List xs) -> do
            if idx < 0 || idx >= fromIntegral (length xs)
                then throwError $ wrongArgumentType ["valid index"]
                else pure $ xs !! fromIntegral idx
        _ -> throwError $ wrongArgumentType ["number", "list"]
nth _ = throwError wrongNumberOfArguments
