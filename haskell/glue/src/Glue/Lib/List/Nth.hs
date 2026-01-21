module Glue.Lib.List.Nth where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

nth :: IR Eval
nth = NativeFunc nthImpl

nthImpl :: IR Eval -> Eval (IR Eval)
nthImpl indexIR = pure $ NativeFunc (nthFrom indexIR)

nthFrom :: IR Eval -> IR Eval -> Eval (IR Eval)
nthFrom indexVal listVal = case (indexVal, listVal) of
    (Integer idx, List xs) -> do
        if idx < 0 || idx >= fromIntegral (length xs)
            then throwError $ wrongArgumentType ["valid index"]
            else pure $ xs !! fromIntegral idx
    _ -> throwError $ wrongArgumentType ["number", "list"]
