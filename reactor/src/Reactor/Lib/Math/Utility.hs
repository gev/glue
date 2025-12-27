module Reactor.Lib.Math.Utility where

import Data.Scientific (fromFloatDigits)
import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Math.Utility.Abs qualified as Abs
import Reactor.Lib.Math.Utility.Ceil qualified as Ceil
import Reactor.Lib.Math.Utility.Floor qualified as Floor
import Reactor.Lib.Math.Utility.Log qualified as Log
import Reactor.Lib.Math.Utility.Max qualified as Max
import Reactor.Lib.Math.Utility.Min qualified as Min
import Reactor.Lib.Math.Utility.Round qualified as Round
import Reactor.Lib.Math.Utility.Trunc qualified as Trunc

utility :: Frame Eval
utility =
    E.frameFromList
        [ ("abs", Native (Func Abs.abs))
        , ("log", Native (Func Log.log))
        , ("floor", Native (Func Floor.floor))
        , ("ceil", Native (Func Ceil.ceil))
        , ("round", Native (Func Round.round))
        , ("trunc", Native (Func Trunc.trunc))
        , ("min", Native (Func Min.min))
        , ("max", Native (Func Max.max))
        , ("pi", Number (fromFloatDigits @Double pi))
        , ("e", Number (fromFloatDigits @Double (exp 1)))
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
