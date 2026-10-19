module Glue.Lib.List.Prepend where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

prepend :: IR Eval
prepend = NativeFunc prependImpl

prependImpl :: IR Eval -> Eval (IR Eval)
prependImpl item = pure $ NativeFunc (prependWith item)

prependWith :: IR Eval -> IR Eval -> Eval (IR Eval)
prependWith Void ir = pure ir
prependWith item list = case list of
    Void -> pure $ List [item]
    List xs -> pure $ List (item : xs)
    _ -> throwError $ wrongArgumentType ["any", "list"]
