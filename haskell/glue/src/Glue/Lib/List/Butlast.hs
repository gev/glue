module Glue.Lib.List.Butlast where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

butlast :: IR Eval
butlast = NativeFunc butlastImpl

butlastImpl :: IR Eval -> Eval (IR Eval)
butlastImpl arg = case arg of
    List [] -> throwError $ wrongArgumentType ["non-empty list"]
    List [_] -> pure $ List []
    List xs -> pure $ List (init xs)
    _ -> throwError $ wrongArgumentType ["list"]
