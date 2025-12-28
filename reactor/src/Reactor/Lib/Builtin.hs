module Reactor.Lib.Builtin where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Builtin.Def (def)
import Reactor.Lib.Builtin.Lambda (lambda)
import Reactor.Lib.Builtin.List (list)
import Reactor.Lib.Builtin.Object (object)
import Reactor.Lib.Builtin.Quote (quote)
import Reactor.Lib.Builtin.Set (set)

builtin :: Frame Eval
builtin =
    E.frameFromList
        [ ("quote", Native (Special quote))
        , ("def", Native (Special def))
        , ("set", Native (Special set))
        , ("lambda", Native (Special lambda))
        , ("\\", Native (Special lambda))
        , ("list", Native (Func list))
        , ("object", Native (Func object))
        ]
