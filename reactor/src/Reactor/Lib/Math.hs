module Reactor.Lib.Math where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Lib.Math.Trigonometric qualified as Trigonometric
import Reactor.Lib.Math.Utility qualified as Utility

math :: Frame Eval
math = E.unionFrames Trigonometric.trigonometric Utility.utility
