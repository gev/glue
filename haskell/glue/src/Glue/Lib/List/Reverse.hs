module Glue.Lib.List.Reverse where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

reverse :: IR Eval
reverse = NativeFunc reverseImpl

reverseImpl :: IR Eval -> Eval (IR Eval)
reverseImpl arg = case arg of
    List xs -> pure $ List (Prelude.reverse xs)
    _ -> throwError $ wrongArgumentType ["list"]
