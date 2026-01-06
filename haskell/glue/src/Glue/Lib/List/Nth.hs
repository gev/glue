module Glue.Lib.List.Nth where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

nth :: [IR Eval] -> Eval (IR Eval)
nth [indexIR, listIR] = do
    indexVal <- evalRequired indexIR
    listVal <- evalRequired listIR
    case (indexVal, listVal) of
        (Number idx, List xs) -> do
            let intIdx = floor idx
            if intIdx < 0 || intIdx >= length xs
                then throwError $ wrongArgumentType ["valid index"]
                else pure $ xs !! intIdx
        _ -> throwError $ wrongArgumentType ["number", "list"]
nth _ = throwError wrongNumberOfArguments
