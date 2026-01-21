module Glue.Lib.List.Flatten where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

flatten :: IR Eval
flatten = NativeFunc flattenImpl

flattenImpl :: IR Eval -> Eval (IR Eval)
flattenImpl list = case list of
    List xs -> do
        flattened <- flattenList xs
        pure $ List flattened
    _ -> throwError $ wrongArgumentType ["list"]

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
