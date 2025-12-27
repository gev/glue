module Reactor.Lib.Math.Arithmetic where

import Reactor.Env qualified as E
import Reactor.Eval (Eval)
import Reactor.IR (Frame, IR (..), Native (..))
import Reactor.Lib.Math.Arithmetic.Add qualified as Add
import Reactor.Lib.Math.Arithmetic.Div qualified as Div
import Reactor.Lib.Math.Arithmetic.Mod qualified as Mod
import Reactor.Lib.Math.Arithmetic.Mul qualified as Mul
import Reactor.Lib.Math.Arithmetic.Sub qualified as Sub

arithmetic :: Frame Eval
arithmetic =
    E.frameFromList
        [ ("+", Native (Func Add.add))
        , ("add", Native (Func Add.add))
        , ("-", Native (Func Sub.sub))
        , ("sub", Native (Func Sub.sub))
        , ("*", Native (Func Mul.mul))
        , ("mul", Native (Func Mul.mul))
        , ("/", Native (Func Div.div))
        , ("div", Native (Func Div.div))
        , ("%", Native (Func Mod.mod))
        , ("mod", Native (Func Mod.mod))
        ]
