module Reactor.Lib.Math where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame)
import Reactor.Lib.Math.Arithmetic qualified as Arithmetic
import Reactor.Lib.Math.Const qualified as Const
import Reactor.Lib.Math.Power qualified as Power
import Reactor.Lib.Math.Trigonometric qualified as Trigonometric
import Reactor.Lib.Math.Utility qualified as Utility

math :: Frame Eval
math =
    E.unionFrames Arithmetic.arithmetic $
        E.unionFrames Const.const $
            E.unionFrames Power.power $
                E.unionFrames Trigonometric.trigonometric Utility.utility
