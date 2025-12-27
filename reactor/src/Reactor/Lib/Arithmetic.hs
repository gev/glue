module Reactor.Lib.Arithmetic where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Arithmetic.Add qualified as Add
import Reactor.Lib.Arithmetic.Sub qualified as Sub

arithmetic :: Frame Eval
arithmetic =
    E.frameFromList
        [ ("+", Native (Func Add.add))
        , ("add", Native (Func Add.add))
        , ("-", Native (Func Sub.sub))
        , ("sub", Native (Func Sub.sub))
        ]
