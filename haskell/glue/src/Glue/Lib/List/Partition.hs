module Glue.Lib.List.Partition where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

partition :: IR Eval
partition = NativeFunc partitionImpl

partitionImpl :: IR Eval -> Eval (IR Eval)
partitionImpl predicateIR = pure $ NativeFunc (partitionList predicateIR)

partitionList :: IR Eval -> IR Eval -> Eval (IR Eval)
partitionList predicateIR listIR = do
    predicate <- eval predicateIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Partition list into two lists based on predicate
            (matching, nonMatching) <- partitionElements predicate xs
            pure $ List [List matching, List nonMatching]
        _ -> throwError $ wrongArgumentType ["function", "list"]

-- Helper function to partition list based on predicate
partitionElements :: IR Eval -> [IR Eval] -> Eval ([IR Eval], [IR Eval])
partitionElements _ [] = pure ([], [])
partitionElements predicate (x : xs) = do
    -- Evaluate (predicate x) and check if it returns true
    result <- eval (List [predicate, x])
    (matching, nonMatching) <- partitionElements predicate xs
    case result of
        Bool True -> pure (x : matching, nonMatching)
        Bool False -> pure (matching, x : nonMatching)
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
