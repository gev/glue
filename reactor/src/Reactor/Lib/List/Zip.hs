module Reactor.Lib.List.Zip where

import Reactor.Eval (Eval, evalRequired, throwError)
import Reactor.Eval.Error (GeneralError (..))
import Reactor.IR (IR (..))

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
