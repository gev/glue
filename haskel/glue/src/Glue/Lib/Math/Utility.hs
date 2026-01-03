module Glue.Lib.Math.Utility where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..), Native (..))
import Glue.Lib.Math.Utility.Abs qualified as Abs
import Glue.Lib.Math.Utility.Ceil qualified as Ceil
import Glue.Lib.Math.Utility.Floor qualified as Floor

import Glue.Lib.Math.Utility.Max qualified as Max
import Glue.Lib.Math.Utility.Min qualified as Min
import Glue.Lib.Math.Utility.Round qualified as Round
import Glue.Lib.Math.Utility.Trunc qualified as Trunc

utility :: Frame Eval
utility =
    E.frameFromList
        [ ("abs", Native (Func Abs.abs))
        , ("floor", Native (Func Floor.floor))
        , ("ceil", Native (Func Ceil.ceil))
        , ("round", Native (Func Round.round))
        , ("trunc", Native (Func Trunc.trunc))
        , ("min", Native (Func Min.min))
        , ("max", Native (Func Max.max))
        ]

-- Note: exp, log, pow, floor, ceil, round, trunc, min, max are not yet integrated
-- expFrame = E.singletonFrame "exp" Exp.exp
-- logFrame = E.singletonFrame "log" Log.log
-- powFrame = E.singletonFrame "pow" Pow.pow
-- floorFrame = E.singletonFrame "floor" Floor.floor
-- ceilFrame = E.singletonFrame "ceil" Ceil.ceil
-- roundFrame = E.singletonFrame "round" Round.round
-- truncFrame = E.singletonFrame "trunc" Trunc.trunc
-- minFrame = E.singletonFrame "min" Min.min
-- maxFrame = E.singletonFrame "max" Max.max
