module Glue.Lib.Math.Arithmetic.Mul where

import Glue.Eval.Exception

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

mul :: [IR Eval] -> Eval (IR Eval)
mul [] = throwError $ wrongArgumentType ["number"]
mul args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError $ wrongArgumentType ["number"]
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ wrongArgumentType ["number"]
                else pure $ Number (product nums)
        _ -> throwError $ wrongArgumentType ["number"]
