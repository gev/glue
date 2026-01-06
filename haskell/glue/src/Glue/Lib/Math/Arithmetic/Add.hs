module Glue.Lib.Math.Arithmetic.Add where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

add :: [IR Eval] -> Eval (IR Eval)
add [] = throwError $ wrongArgumentType ["number"]
add args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError $ wrongArgumentType ["number"]
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ wrongArgumentType ["number"]
                else pure $ Number (sum nums)
        _ -> throwError $ wrongArgumentType ["number"]
