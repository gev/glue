module Reactor.Lib.List.Flatten where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

flatten :: [IR Eval] -> Eval (IR Eval)
flatten [listIR] = do
    list <- evalRequired listIR
    case list of
        List xs -> do
            flattened <- flattenList xs
            pure $ List flattened
        _ -> throwError $ WrongArgumentType ["list"]
flatten _ = throwError WrongNumberOfArguments

-- Helper function to flatten a list recursively
flattenList :: [IR Eval] -> Eval [IR Eval]
flattenList [] = pure []
flattenList (x : xs) = do
    case x of
        List ys -> do
            flattenedHead <- flattenList ys
            flattenedTail <- flattenList xs
            pure $ flattenedHead ++ flattenedTail
        _ -> do
            flattenedTail <- flattenList xs
            pure $ x : flattenedTail
