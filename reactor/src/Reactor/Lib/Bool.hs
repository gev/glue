module Reactor.Lib.Bool where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Bool.Eq (eq)
import Reactor.Lib.Bool.Error (BoolError (..))
import Reactor.Lib.Bool.Ge (ge)
import Reactor.Lib.Bool.Gt (gt)
import Reactor.Lib.Bool.If (if_)
import Reactor.Lib.Bool.Le (le)
import Reactor.Lib.Bool.Lt (lt)
import Reactor.Lib.Bool.Ne (ne)
import Reactor.Lib.Bool.Not (not_)
import Reactor.Lib.Bool.Until (until_)
import Reactor.Lib.Bool.When (when_)
import Reactor.Lib.Bool.While (while_)

bool :: Frame Eval
bool =
    E.frameFromList
        [ ("true", Symbol "true")
        , ("false", Symbol "false")
        , ("eq", Native (Func eq))
        , ("==", Native (Func eq))
        , ("ne", Native (Func ne))
        , ("\\=", Native (Func ne))
        , ("lt", Native (Func lt))
        , ("<", Native (Func lt))
        , ("le", Native (Func le))
        , ("<=", Native (Func le))
        , ("gt", Native (Func gt))
        , (">", Native (Func gt))
        , ("ge", Native (Func ge))
        , (">=", Native (Func ge))
        , ("not", Native (Func not_))
        , ("!", Native (Func not_))
        , ("if", Native (Special if_))
        , ("when", Native (Special when_))
        , ("while", Native (Special while_))
        , ("until", Native (Special until_))
        ]
