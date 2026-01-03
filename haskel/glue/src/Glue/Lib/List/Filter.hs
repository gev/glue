module Glue.Lib.List.Filter where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
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
        _ -> throwError $ WrongArgumentType ["function", "list"]
filter _ = throwError WrongNumberOfArguments

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
    result <- eval (List [pred, x]) >>= maybe (throwError ExpectedValue) pure
    case result of
        Symbol "true" -> pure True
        Symbol "false" -> pure False
        _ -> throwError $ WrongArgumentType ["boolean result from predicate"]
