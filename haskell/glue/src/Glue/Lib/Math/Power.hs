module Glue.Lib.Math.Power where

import Glue.Eval (Eval)
import Glue.Lib.Math.Power.Exp qualified as Exp
import Glue.Lib.Math.Power.Pow qualified as Pow
import Glue.Lib.Math.Power.Sqrt qualified as Sqrt
import Glue.Module (ModuleInfo, nativeModule)

power :: ModuleInfo Eval
power =
    nativeModule
        "ffi.math.power"
        [ ("exp", Exp.exp)
        , ("pow", Pow.pow)
        , ("sqrt", Sqrt.sqrt)
        ]
