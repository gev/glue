module Reactor.Lib.List where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.List.Append (append)
import Reactor.Lib.List.Butlast (butlast)
import Reactor.Lib.List.Car (car)
import Reactor.Lib.List.Cdr (cdr)
import Reactor.Lib.List.Cons (cons)
import Reactor.Lib.List.Filter qualified as Filter
import Reactor.Lib.List.Last qualified as Last
import Reactor.Lib.List.Length qualified as Length
import Reactor.Lib.List.Map qualified as Map
import Reactor.Lib.List.Member (member)
import Reactor.Lib.List.Nth (nth)
import Reactor.Lib.List.Remove (remove)
import Reactor.Lib.List.Reverse qualified as Reverse

list :: Frame Eval
list =
    E.frameFromList
        [ ("append", Native (Func append))
        , ("butlast", Native (Func butlast))
        , ("car", Native (Func car))
        , ("cdr", Native (Func cdr))
        , ("cons", Native (Func cons))
        , ("filter", Native (Func Filter.filter))
        , ("last", Native (Func Last.last))
        , ("length", Native (Func Length.length))
        , ("map", Native (Func Map.map))
        , ("member", Native (Func member))
        , ("nth", Native (Func nth))
        , ("remove", Native (Func remove))
        , ("reverse", Native (Func Reverse.reverse))
        ]
