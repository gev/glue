module Glue.Lib.List.Head where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

head :: IR Eval
head = NativeFunc headImpl

headImpl :: IR Eval -> Eval (IR Eval)
headImpl arg = case arg of
    Void -> pure Void
    List [] -> pure Void
    List (x : _) -> pure x
    _ -> throwError $ wrongArgumentType ["list"]
