module Reactor.Lib where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Lib.Bool qualified as Bool
import Reactor.Lib.Builtin qualified as Builtin
import Reactor.Lib.Math qualified as Math
import Reactor.Lib.Math.Arithmetic qualified as Arithmetic

lib :: Frame Eval
lib = E.unionFrames Builtin.builtin (E.unionFrames Bool.bool (E.unionFrames Arithmetic.arithmetic Math.math))
