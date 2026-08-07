module Glue.Lib.List.Prepand where

import Glue.Eval (Eval, throwError)
import Glue.Eval.Exception (wrongArgumentType)
import Glue.IR (IR (..))

prepand :: IR Eval
prepand = NativeFunc prepandImpl

prepandImpl :: IR Eval -> Eval (IR Eval)
prepandImpl item = pure $ NativeFunc (prepandWith item)

prepandWith :: IR Eval -> IR Eval -> Eval (IR Eval)
prepandWith item list = case list of
    List xs -> pure $ List (item : xs)
    _ -> throwError $ wrongArgumentType ["list"]
