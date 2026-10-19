module Glue.Lib.List.Tail where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

tail :: IR Eval
tail = NativeFunc tailImpl

tailImpl :: IR Eval -> Eval (IR Eval)
tailImpl arg = case arg of
    Void -> pure Void
    List (_ : xs) -> pure $ List xs
    List [] -> pure $ List []
    _ -> throwError $ wrongArgumentType ["list"]
