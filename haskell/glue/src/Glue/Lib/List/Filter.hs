module Glue.Lib.List.Filter where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

filter :: [IR Eval] -> Eval (IR Eval)
filter [predIR, listIR] = do
    pred <- evalRequired predIR
    list <- evalRequired listIR
    case list of
        List xs -> do
            -- Filter elements that satisfy the predicate
            filtered <- filterElements pred xs
            pure $ List filtered
        _ -> throwError $ wrongArgumentType ["function", "list"]
filter _ = throwError wrongNumberOfArguments

-- Helper function to filter elements
filterElements :: IR Eval -> [IR Eval] -> Eval [IR Eval]
filterElements _ [] = pure []
filterElements pred (x : xs) = do
    satisfies <- applyPredicate pred x
    rest <- filterElements pred xs
    if satisfies
        then pure (x : rest)
        else pure rest

-- Helper function to apply predicate to an element
applyPredicate :: IR Eval -> IR Eval -> Eval Bool
applyPredicate pred x = do
    -- Evaluate (pred x) and check if it returns true
    result <- eval (List [pred, x])
    case result of
        Bool True -> pure True
        Bool False -> pure False
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
