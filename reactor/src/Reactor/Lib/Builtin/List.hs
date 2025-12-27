module Reactor.Lib.Builtin.List where

import Reactor.Eval (Eval)
import Reactor.IR (IR (..))

list :: [IR Eval] -> Eval (IR Eval)
list args = pure (List args)
