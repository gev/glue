module Glue.Lib.List.Zip where

import Glue.Eval (Eval, eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

zip :: IR Eval
zip = NativeFunc zipImpl

zipImpl :: [IR Eval] -> Eval (IR Eval)
zipImpl [list1IR, list2IR] = do
    list1 <- eval list1IR
    list2 <- eval list2IR
    case (list1, list2) of
        (List xs, List ys) -> do
            let zipped = zipLists xs ys
            pure $ List zipped
        _ -> throwError $ wrongArgumentType ["list", "list"]
zipImpl _ = throwError wrongNumberOfArguments

-- Helper function to zip two lists
zipLists :: [IR Eval] -> [IR Eval] -> [IR Eval]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x : xs) (y : ys) = List [x, y] : zipLists xs ys
