module Glue.Lib.List.ZipWith where

import Glue.Eval (Eval, apply, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

zipWith :: IR Eval
zipWith = NativeFunc zipWithImpl

zipWithImpl :: IR Eval -> Eval (IR Eval)
zipWithImpl funcIR = pure $ NativeFunc (zipWithList1 funcIR)

zipWithList1 :: IR Eval -> IR Eval -> Eval (IR Eval)
zipWithList1 funcIR list1IR = pure $ NativeFunc (zipWithLists funcIR list1IR)

zipWithLists :: IR Eval -> IR Eval -> IR Eval -> Eval (IR Eval)
zipWithLists func list1 list2 = case (list1, list2) of
    (Void, _) -> pure Void
    (_, Void) -> pure Void
    (List xs, List ys) -> List <$> zipWithElements func xs ys
    _ -> throwError $ wrongArgumentType ["function", "list", "list"]

zipWithElements :: IR Eval -> [IR Eval] -> [IR Eval] -> Eval [IR Eval]
zipWithElements _ [] _ = pure []
zipWithElements _ _ [] = pure []
zipWithElements func (x : xs) (y : ys) = do
    res <- apply func [x, y]
    rest <- zipWithElements func xs ys
    pure (res : rest)
