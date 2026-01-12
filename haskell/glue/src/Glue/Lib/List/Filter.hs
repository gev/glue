module Glue.Lib.List.Filter where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception (wrongArgumentType, wrongNumberOfArguments)
import Glue.IR (IR (..))

filter :: [IR Eval] -> Eval (IR Eval)
filter [predicateIR, listIR] = do
    predicate <- eval predicateIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Filter elements that satisfy the predicate
            filtered <- filterElements predicate xs
            pure $ List filtered
        _ -> throwError $ wrongArgumentType ["function", "list"]
filter _ = throwError wrongNumberOfArguments

-- Helper function to filter elements
filterElements :: IR Eval -> [IR Eval] -> Eval [IR Eval]
filterElements _ [] = pure []
filterElements predicate (x : xs) = do
    satisfies <- applyPredicate predicate x
    rest <- filterElements predicate xs
    if satisfies
        then pure (x : rest)
        else pure rest

-- Helper function to apply predicate to an element
applyPredicate :: IR Eval -> IR Eval -> Eval Bool
applyPredicate predicate x = do
    -- Evaluate (pred x) and check if it returns true
    result <- eval (List [predicate, x])
    case result of
        Bool True -> pure True
        Bool False -> pure False
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
