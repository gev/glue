module Glue.Lib.List.Append where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

append :: IR Eval
append = NativeFunc appendImpl

appendImpl :: IR Eval -> Eval (IR Eval)
appendImpl list = pure $ NativeFunc (appendTo list)

appendTo :: IR Eval -> IR Eval -> Eval (IR Eval)
appendTo list item = case list of
    List xs -> pure $ List (xs <> [item])
    _ -> throwError $ wrongArgumentType ["list", "item"]
