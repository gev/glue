module Glue.Lib.List.Last where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

last :: IR Eval
last = NativeFunc lastImpl

lastImpl :: IR Eval -> Eval (IR Eval)
lastImpl arg = case arg of
    List [] -> throwError $ wrongArgumentType ["non-empty list"]
    List xs -> pure $ Prelude.last xs
    _ -> throwError $ wrongArgumentType ["list"]
