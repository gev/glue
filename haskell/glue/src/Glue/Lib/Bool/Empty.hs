module Glue.Lib.Bool.Empty where

import Glue.Eval (Eval)
import Glue.IR as IR (IR (..), empty)

empty :: IR Eval
empty = NativeFunc emptyImpl

emptyImpl :: IR Eval -> Eval (IR Eval)
emptyImpl = pure . Bool . IR.empty
