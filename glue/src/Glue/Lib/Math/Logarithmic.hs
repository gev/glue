module Glue.Lib.Math.Logarithmic where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..), Native (..))
import Glue.Lib.Math.Logarithmic.Lg qualified as Lg
import Glue.Lib.Math.Logarithmic.Ln qualified as Ln
import Glue.Lib.Math.Logarithmic.Log qualified as Log

logarithmic :: Frame Eval
logarithmic =
    E.frameFromList
        [ ("log", Native (Func Log.log))
        , ("ln", Native (Func Ln.ln))
        , ("lg", Native (Func Lg.lg))
        ]
