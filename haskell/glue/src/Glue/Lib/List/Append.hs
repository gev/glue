module Glue.Lib.List.Append where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

append :: IR Eval
append = NativeFunc appendImpl

appendImpl :: IR Eval -> Eval (IR Eval)
appendImpl list1 = pure $ NativeFunc (appendTo list1)

appendTo :: IR Eval -> IR Eval -> Eval (IR Eval)
appendTo list1 list2 = case (list1, list2) of
    (List xs, List ys) -> pure $ List (xs ++ ys)
    _ -> throwError $ wrongArgumentType ["list", "list"]
