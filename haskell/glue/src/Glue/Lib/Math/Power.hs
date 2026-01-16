module Glue.Lib.Math.Power where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Lib.Math.Power.Exp qualified as Exp
import Glue.Lib.Math.Power.Pow qualified as Pow
import Glue.Lib.Math.Power.Sqrt qualified as Sqrt
import Glue.Module (ModuleInfo, nativeModule)

power :: ModuleInfo Eval
power =
    nativeModule
        "ffi.math.power"
        [ ("exp", NativeFunc Exp.exp)
        , ("pow", NativeFunc Pow.pow)
        , ("sqrt", NativeFunc Sqrt.sqrt)
        ]
