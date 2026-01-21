module Glue.Lib.List.Car where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

car :: IR Eval
car = NativeFunc carImpl

carImpl :: IR Eval -> Eval (IR Eval)
carImpl arg = case arg of
    List (x : _) -> pure x
    List [] -> throwError $ wrongArgumentType ["non-empty list"]
    _ -> throwError $ wrongArgumentType ["list"]
