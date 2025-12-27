module Reactor.Lib.Math.Power where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Math.Power.Exp qualified as Exp
import Reactor.Lib.Math.Power.Pow qualified as Pow
import Reactor.Lib.Math.Power.Sqrt qualified as Sqrt

power :: Frame Eval
power =
    E.frameFromList
        [ ("exp", Native (Func Exp.exp))
        , ("pow", Native (Func Pow.pow))
        , ("sqrt", Native (Func Sqrt.sqrt))
        ]
