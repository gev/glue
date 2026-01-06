module Glue.Lib.Math where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame)
import Glue.Lib.Math.Arithmetic qualified as Arithmetic
import Glue.Lib.Math.Const qualified as Const
import Glue.Lib.Math.Logarithmic qualified as Logarithmic
import Glue.Lib.Math.Power qualified as Power
import Glue.Lib.Math.Trigonometric qualified as Trigonometric
import Glue.Lib.Math.Utility qualified as Utility

math :: Frame Eval
math =
    E.unionFramesList
        [ Arithmetic.arithmetic
        , Const.const
        , Logarithmic.logarithmic
        , Trigonometric.trigonometric
        , Utility.utility
        , Power.power
        ]
