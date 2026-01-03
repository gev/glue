module Glue.Lib.Math.Arithmetic where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..), Native (..))
import Glue.Lib.Math.Arithmetic.Add qualified as Add
import Glue.Lib.Math.Arithmetic.Div qualified as Div
import Glue.Lib.Math.Arithmetic.Mod qualified as Mod
import Glue.Lib.Math.Arithmetic.Mul qualified as Mul
import Glue.Lib.Math.Arithmetic.Sub qualified as Sub

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
