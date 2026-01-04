module Glue.Lib.List.Position where

import Glue.Eval (Eval, eval, evalRequired, throwError)
import Glue.Eval.Error (GeneralError (..))
import Glue.IR (IR (..))

position :: [IR Eval] -> Eval (IR Eval)
position [predIR, listIR] = do
    pred <- evalRequired predIR
    list <- evalRequired listIR
    case list of
        List xs -> do
            -- Find index of first element that satisfies predicate
            findPosition pred xs 0
        _ -> throwError $ WrongArgumentType ["function", "list"]
position _ = throwError WrongNumberOfArguments

-- Helper function to find position of first element satisfying predicate
findPosition :: IR Eval -> [IR Eval] -> Int -> Eval (IR Eval)
findPosition _ [] _ = throwError $ WrongArgumentType ["element satisfying predicate"]
findPosition pred (x : xs) idx = do
    -- Evaluate (pred x) and check if it returns true
    result <- eval (List [pred, x]) >>= maybe (throwError ExpectedValue) pure
    case result of
        Bool True -> pure $ Number (fromIntegral idx)
        Bool False -> findPosition pred xs (idx + 1)
        _ -> throwError $ WrongArgumentType ["boolean result from predicate"]
