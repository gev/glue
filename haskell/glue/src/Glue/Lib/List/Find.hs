module Glue.Lib.List.Find where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

find :: [IR Eval] -> Eval (IR Eval)
find [predicateIR, listIR] = do
    predicate <- eval predicateIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Find first element that satisfies predicate
            findElement predicate xs
        _ -> throwError $ wrongArgumentType ["function", "list"]
find _ = throwError wrongNumberOfArguments

-- Helper function to find first element satisfying predicate
findElement :: IR Eval -> [IR Eval] -> Eval (IR Eval)
findElement _ [] = throwError $ wrongArgumentType ["element satisfying predicate"]
findElement predicate (x : xs) = do
    -- Evaluate (predicate x) and check if it returns true
    result <- eval (List [predicate, x])
    case result of
        Bool True -> pure x
        Bool False -> findElement predicate xs
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
