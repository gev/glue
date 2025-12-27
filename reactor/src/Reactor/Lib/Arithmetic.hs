module Reactor.Lib.Arithmetic where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Arithmetic.Add qualified as Add

arithmetic :: Frame Eval
arithmetic =
    E.frameFromList
        [ ("+", Native (Func Add.add))
        , ("add", Native (Func Add.add))
        ]
