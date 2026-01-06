module Glue.Lib.Math.Arithmetic.Div where

import Glue.Eval.Exception

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

div :: [IR Eval] -> Eval (IR Eval)
div [] = throwError $ wrongArgumentType ["number"]
div [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            if n == 0
                then throwError divByZero
                else pure $ Number (1 / n)
        _ -> throwError $ wrongArgumentType ["number"]
div args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError wrongNumberOfArguments
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ wrongArgumentType ["number"]
                else do
                    let result = foldl (/) first (tail nums)
                    if 0 `elem` tail nums
                        then throwError divByZero
                        else pure $ Number result
        _ -> throwError $ wrongArgumentType ["number"]
