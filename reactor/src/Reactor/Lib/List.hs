module Reactor.Lib.List where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.List.Append (append)
import Reactor.Lib.List.Car (car)
import Reactor.Lib.List.Cdr (cdr)
import Reactor.Lib.List.Cons (cons)
import Reactor.Lib.List.Length qualified as Length
import Reactor.Lib.List.Nth (nth)
import Reactor.Lib.List.Reverse qualified as Reverse

list :: Frame Eval
list =
    E.frameFromList
        [ ("append", Native (Func append))
        , ("car", Native (Func car))
        , ("cdr", Native (Func cdr))
        , ("cons", Native (Func cons))
        , ("length", Native (Func Length.length))
        , ("nth", Native (Func nth))
        , ("reverse", Native (Func Reverse.reverse))
        ]
