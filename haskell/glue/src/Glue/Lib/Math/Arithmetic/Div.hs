module Glue.Lib.Math.Arithmetic.Div where

import Glue.Eval.Exception (RuntimeException (..))

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.IR (IR (..))

div :: [IR Eval] -> Eval (IR Eval)
div [] = throwError $ WrongArgumentType ["number"]
div [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            if n == 0
                then throwError DivByZero
                else pure $ Number (1 / n)
        _ -> throwError $ WrongArgumentType ["number"]
div args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError WrongNumberOfArguments
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ WrongArgumentType ["number"]
                else do
                    let result = foldl (/) first (tail nums)
                    if 0 `elem` tail nums
                        then throwError DivByZero
                        else pure $ Number result
        _ -> throwError $ WrongArgumentType ["number"]
