module Glue.Lib.Bool.IsEmpty where

import Glue.Eval (Eval)
import Glue.IR (IR (..), isEmpty)

isEmpty_ :: IR Eval
isEmpty_ = NativeFunc isEmptyImpl

isEmptyImpl :: IR Eval -> Eval (IR Eval)
isEmptyImpl = pure . Bool . isEmpty
