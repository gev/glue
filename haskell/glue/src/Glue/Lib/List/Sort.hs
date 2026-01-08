module Glue.Lib.List.Sort where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception
import Glue.IR (IR (..))

sort :: [IR Eval] -> Eval (IR Eval)
sort [listIR] = case listIR of
    List xs -> do
        sorted <- sortList xs
        pure $ List sorted
    _ -> throwError $ wrongArgumentType ["list"]
sort _ = throwError wrongNumberOfArguments

-- Helper function to sort a list using merge sort
sortList :: [IR Eval] -> Eval [IR Eval]
sortList [] = pure []
sortList [x] = pure [x]
sortList xs = do
    let (left, right) = splitAt (length xs `div` 2) xs
    sortedLeft <- sortList left
    sortedRight <- sortList right
    merge sortedLeft sortedRight

-- Helper function to merge two sorted lists
merge :: [IR Eval] -> [IR Eval] -> Eval [IR Eval]
merge [] ys = pure ys
merge xs [] = pure xs
merge (x : xs) (y : ys) = do
    cmp <- compareIR x y
    case cmp of
        LT -> do
            rest <- merge xs (y : ys)
            pure (x : rest)
        _ -> do
            rest <- merge (x : xs) ys
            pure (y : rest)

-- Helper function to compare two IR values
compareIR :: IR Eval -> IR Eval -> Eval Ordering
compareIR (Integer a) (Integer b) = pure (compare a b)
compareIR (Float a) (Float b) = pure (compare a b)
compareIR (Integer a) (Float b) = pure (compare (fromIntegral a) b)
compareIR (Float a) (Integer b) = pure (compare a (fromIntegral b))
compareIR (String a) (String b) = pure (compare a b)
compareIR (Symbol a) (Symbol b) = pure (compare a b)
compareIR _ _ = throwError $ wrongArgumentType ["comparable values (numbers, strings, or symbols)"]
