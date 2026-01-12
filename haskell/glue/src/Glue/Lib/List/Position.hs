module Glue.Lib.List.Position where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

position :: [IR Eval] -> Eval (IR Eval)
position [predicateIR, listIR] = do
    predicate <- eval predicateIR
    list <- eval listIR
    case list of
        List xs -> do
            -- Find index of first element that satisfies predicate
            findPosition predicate xs 0
        _ -> throwError $ wrongArgumentType ["function", "list"]
position _ = throwError wrongNumberOfArguments

-- Helper function to find position of first element satisfying predicate
findPosition :: IR Eval -> [IR Eval] -> Int -> Eval (IR Eval)
findPosition _ [] _ = throwError $ wrongArgumentType ["element satisfying predicate"]
findPosition predicate (x : xs) idx = do
    -- Evaluate (predicate x) and check if it returns true
    result <- eval (List [predicate, x])
    case result of
        Bool True -> pure $ Integer (fromIntegral idx)
        Bool False -> findPosition predicate xs (idx + 1)
        _ -> throwError $ wrongArgumentType ["boolean result from predicate"]
