module Glue.Lib.List.Concat where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

concat :: IR Eval
concat = NativeFunc concatImpl

concatImpl :: IR Eval -> Eval (IR Eval)
concatImpl list1 = pure $ NativeFunc (concatTo list1)

concatTo :: IR Eval -> IR Eval -> Eval (IR Eval)
concatTo list1 list2 = case (list1, list2) of
    (List xs, List ys) -> pure $ List (xs <> ys)
    _ -> throwError $ wrongArgumentType ["list", "list"]
