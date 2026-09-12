module Glue.Lib.Bool.Exists where

import Glue.Eval (Eval)
import Glue.IR as IR (IR (..), exists)

exists :: IR Eval
exists = NativeFunc existsImpl

existsImpl :: IR Eval -> Eval (IR Eval)
existsImpl = pure . Bool . IR.exists
