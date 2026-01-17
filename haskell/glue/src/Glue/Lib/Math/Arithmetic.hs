module Glue.Lib.Math.Arithmetic where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Lib.Math.Arithmetic.Add qualified as Add
import Glue.Lib.Math.Arithmetic.Div qualified as Div
import Glue.Lib.Math.Arithmetic.Mod qualified as Mod
import Glue.Lib.Math.Arithmetic.Mul qualified as Mul
import Glue.Lib.Math.Arithmetic.Sub qualified as Sub
import Glue.Module (ModuleInfo, nativeModule)

arithmetic :: ModuleInfo Eval
arithmetic =
    nativeModule
        "ffi.math.arithmetic"
        [ ("+", NativeFunc Add.add)
        , ("add", NativeFunc Add.add)
        , ("-", NativeFunc Sub.sub)
        , ("sub", NativeFunc Sub.sub)
        , ("*", NativeFunc Mul.mul)
        , ("mul", NativeFunc Mul.mul)
        , ("/", NativeFunc Div.div)
        , ("div", NativeFunc Div.div)
        , ("%", NativeFunc Mod.mod)
        , ("mod", NativeFunc Mod.mod)
        ]
