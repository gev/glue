module Glue.Lib.List.Zip where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

zip :: IR Eval
zip = NativeFunc zipImpl

zipImpl :: IR Eval -> Eval (IR Eval)
zipImpl list1IR = pure $ NativeFunc (zipLists list1IR)

zipLists :: IR Eval -> IR Eval -> Eval (IR Eval)
zipLists list1 list2 = case (list1, list2) of
    (List xs, List ys) -> do
        let zipped = zipElements xs ys
        pure $ List zipped
    _ -> throwError $ wrongArgumentType ["list", "list"]

-- Helper function to zip two lists
zipElements :: [IR Eval] -> [IR Eval] -> [IR Eval]
zipElements [] _ = []
zipElements _ [] = []
zipElements (x : xs) (y : ys) = List [x, y] : zipElements xs ys
