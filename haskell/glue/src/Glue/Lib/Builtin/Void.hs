module Glue.Lib.Builtin.Void (void) where

import Glue.Eval (Eval)
import Glue.IR (IR (..))

-- | Void special form - returns a void
void :: IR Eval
void = Void
