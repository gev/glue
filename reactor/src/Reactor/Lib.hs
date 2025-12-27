module Reactor.Lib where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Lib.Bool qualified as Bool
import Reactor.Lib.Builtin qualified as Builtin

lib :: Frame Eval
lib = E.unionFrames Builtin.builtin Bool.bool
