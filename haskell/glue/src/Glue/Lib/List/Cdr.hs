module Glue.Lib.List.Cdr where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

cdr :: IR Eval
cdr = NativeFunc cdrImpl

cdrImpl :: IR Eval -> Eval (IR Eval)
cdrImpl arg = case arg of
    List (_ : xs) -> pure $ List xs
    List [] -> throwError $ wrongArgumentType ["non-empty list"]
    _ -> throwError $ wrongArgumentType ["list"]
