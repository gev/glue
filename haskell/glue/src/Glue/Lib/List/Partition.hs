module Glue.Lib.List.Partition where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

partition :: [IR Eval] -> Eval (IR Eval)
partition [predIR, listIR] = do
    pred <- eval predIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Partition list into two lists based on predicate
            (matching, nonMatching) <- partitionList pred xs
            pure $ List [List matching, List nonMatching]
        _ -> throwError $ wrongArgumentType ["function", "list"]
partition _ = throwError wrongNumberOfArguments

-- Helper function to partition list based on predicate
partitionList :: IR Eval -> [IR Eval] -> Eval ([IR Eval], [IR Eval])
partitionList _ [] = pure ([], [])
partitionList pred (x : xs) = do
    -- Evaluate (pred x) and check if it returns true
    result <- eval (List [pred, x])
    (matching, nonMatching) <- partitionList pred xs
    case result of
        Bool True -> pure (x : matching, nonMatching)
        Bool False -> pure (matching, x : nonMatching)
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
