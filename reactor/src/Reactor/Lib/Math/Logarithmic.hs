module Reactor.Lib.Math.Logarithmic where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Math.Logarithmic.Lg qualified as Lg
import Reactor.Lib.Math.Logarithmic.Ln qualified as Ln
import Reactor.Lib.Math.Logarithmic.Log qualified as Log

logarithmic :: Frame Eval
logarithmic =
    E.frameFromList
        [ ("log", Native (Func Log.log))
        , ("ln", Native (Func Ln.ln))
        , ("lg", Native (Func Lg.lg))
        ]
