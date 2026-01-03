module Glue.Lib.Math.Power where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..), Native (..))
import Glue.Lib.Math.Power.Exp qualified as Exp
import Glue.Lib.Math.Power.Pow qualified as Pow
import Glue.Lib.Math.Power.Sqrt qualified as Sqrt

power :: Frame Eval
power =
    E.frameFromList
        [ ("exp", Native (Func Exp.exp))
        , ("pow", Native (Func Pow.pow))
        , ("sqrt", Native (Func Sqrt.sqrt))
        ]
