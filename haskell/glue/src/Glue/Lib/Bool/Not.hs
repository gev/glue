module Glue.Lib.Bool.Not where

import Glue.Eval (Eval)
import Glue.IR (IR (..))

not_ :: IR Eval
not_ = NativeFunc notImpl

notImpl :: IR Eval -> Eval (IR Eval)
notImpl arg = case arg of
    Bool False -> pure $ Bool True
    _ -> pure $ Bool False
