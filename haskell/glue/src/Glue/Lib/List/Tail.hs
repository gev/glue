module Glue.Lib.List.Tail where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

tail :: IR Eval
tail = NativeFunc tailImpl

tailImpl :: IR Eval -> Eval (IR Eval)
tailImpl arg = case arg of
    List (_ : xs) -> pure $ List xs
    List [] -> throwError $ wrongArgumentType ["non-empty list"]
    _ -> throwError $ wrongArgumentType ["list"]
