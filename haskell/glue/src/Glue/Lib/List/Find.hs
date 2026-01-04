module Glue.Lib.List.Find where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

find :: [IR Eval] -> Eval (IR Eval)
find [predIR, listIR] = do
    pred <- evalRequired predIR
    list <- evalRequired listIR
    case list of
        List xs -> do
            -- Find first element that satisfies predicate
            findElement pred xs
        _ -> throwError $ WrongArgumentType ["function", "list"]
find _ = throwError WrongNumberOfArguments

-- Helper function to find first element satisfying predicate
findElement :: IR Eval -> [IR Eval] -> Eval (IR Eval)
findElement _ [] = throwError $ WrongArgumentType ["element satisfying predicate"]
findElement pred (x : xs) = do
    -- Evaluate (pred x) and check if it returns true
    result <- eval (List [pred, x]) >>= maybe (throwError ExpectedValue) pure
    case result of
        Bool True -> pure x
        Bool False -> findElement pred xs
        _ -> throwError $ WrongArgumentType ["boolean result from predicate"]
