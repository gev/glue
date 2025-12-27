module Reactor.Lib where

import Data.Map qualified as Map
import Reactor.Eval (Eval)
import Reactor.IR (Env)
import Reactor.Lib.Bool qualified as Bool
import Reactor.Lib.Builtin qualified as Builtin

lib :: Env Eval
lib = case (Builtin.builtin, Bool.bool) of
    ([m1], [m2]) -> [Map.union m1 m2]
    _ -> error "Unexpected environment structure"
