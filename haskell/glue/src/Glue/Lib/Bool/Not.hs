module Glue.Lib.Bool.Not where

import Glue.Eval (Eval)
import Glue.IR (IR (..), isFalsy)

not_ :: IR Eval
not_ = NativeFunc notImpl

notImpl :: IR Eval -> Eval (IR Eval)
notImpl = pure . Bool . isFalsy
