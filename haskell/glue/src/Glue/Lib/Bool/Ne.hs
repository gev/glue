module Glue.Lib.Bool.Ne where

import Glue.Eval (Eval)
import Glue.IR (IR (..))

ne :: IR Eval
ne = NativeFunc neImpl

neImpl :: IR Eval -> Eval (IR Eval)
neImpl a = pure $ NativeFunc (neRight a)

neRight :: IR Eval -> IR Eval -> Eval (IR Eval)
neRight a b = pure . Bool $ a /= b
