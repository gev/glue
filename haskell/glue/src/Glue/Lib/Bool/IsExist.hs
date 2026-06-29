module Glue.Lib.Bool.IsExist where

import Glue.Eval (Eval)
import Glue.IR (IR (..), isExist)

isExist_ :: IR Eval
isExist_ = NativeFunc isExistImpl

isExistImpl :: IR Eval -> Eval (IR Eval)
isExistImpl = pure . Bool . isExist
