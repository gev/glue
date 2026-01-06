module Glue.Lib.List.Zip where

import Glue.Eval (Eval, evalRequired, throwError)
import Glue.Eval.Exception (RuntimeException (..))
import Glue.IR (IR (..))

zip :: [IR Eval] -> Eval (IR Eval)
zip [list1IR, list2IR] = do
    list1 <- evalRequired list1IR
    list2 <- evalRequired list2IR
    case (list1, list2) of
        (List xs, List ys) -> do
            let zipped = zipLists xs ys
            pure $ List zipped
        _ -> throwError $ WrongArgumentType ["list", "list"]
zip _ = throwError WrongNumberOfArguments

-- Helper function to zip two lists
zipLists :: [IR Eval] -> [IR Eval] -> [IR Eval]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x : xs) (y : ys) = List [x, y] : zipLists xs ys
