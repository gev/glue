module Glue.Lib.List.Prepend where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

prepend :: IR Eval
prepend = NativeFunc prependImpl

prependImpl :: IR Eval -> Eval (IR Eval)
prependImpl item = pure $ NativeFunc (prependWith item)

prependWith :: IR Eval -> IR Eval -> Eval (IR Eval)
prependWith item list = case list of
    List xs -> pure $ List (item : xs)
    _ -> throwError $ wrongArgumentType ["any", "list"]
