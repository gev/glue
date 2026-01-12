module Glue.Lib.List.Partition where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

partition :: [IR Eval] -> Eval (IR Eval)
partition [predicateIR, listIR] = do
    predicate <- eval predicateIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Partition list into two lists based on predicate
            (matching, nonMatching) <- partitionList predicate xs
            pure $ List [List matching, List nonMatching]
        _ -> throwError $ wrongArgumentType ["function", "list"]
partition _ = throwError wrongNumberOfArguments

-- Helper function to partition list based on predicate
partitionList :: IR Eval -> [IR Eval] -> Eval ([IR Eval], [IR Eval])
partitionList _ [] = pure ([], [])
partitionList predicate (x : xs) = do
    -- Evaluate (predicate x) and check if it returns true
    result <- eval (List [predicate, x])
    (matching, nonMatching) <- partitionList predicate xs
    case result of
        Bool True -> pure (x : matching, nonMatching)
        Bool False -> pure (matching, x : nonMatching)
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
