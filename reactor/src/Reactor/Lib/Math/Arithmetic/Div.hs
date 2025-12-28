module Reactor.Lib.Math.Arithmetic.Div where

import Reactor.Eval.Error (GeneralError (..))

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.IR (IR (..))

div :: [IR Eval] -> Eval (IR Eval)
div [] = throwError WrongNumberOfArguments
div [arg] = do
    va <- evalRequired arg
    case va of
        Number n -> do
            if n == 0
                then throwError DivByZero
                else pure $ Number (1 / n)
        _ -> throwError $ WrongArgumentType "/" ["number"]
div args = do
    values <- mapM evalRequired args
    case values of
        [] -> throwError WrongNumberOfArguments
        (Number first : rest) -> do
            let nums = first : [n | Number n <- rest]
            if length nums /= length values
                then throwError $ WrongArgumentType "/" ["number"]
                else do
                    let result = foldl (/) first (tail nums)
                    if 0 `elem` tail nums
                        then throwError DivByZero
                        else pure $ Number result
        _ -> throwError $ WrongArgumentType "/" ["number"]
