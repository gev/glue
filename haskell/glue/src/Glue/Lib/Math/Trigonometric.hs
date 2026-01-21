module Glue.Lib.Math.Trigonometric where

import Glue.Eval (Eval)
import Glue.Lib.Math.Trigonometric.Acos qualified as Acos
import Glue.Lib.Math.Trigonometric.Asin qualified as Asin
import Glue.Lib.Math.Trigonometric.Atan qualified as Atan
import Glue.Lib.Math.Trigonometric.Cos qualified as Cos
import Glue.Lib.Math.Trigonometric.Sin qualified as Sin
import Glue.Lib.Math.Trigonometric.Tan qualified as Tan
import Glue.Module (ModuleInfo, nativeModule)

trigonometric :: ModuleInfo Eval
trigonometric =
    nativeModule
        "ffi.math.trigonometric"
        [ ("sin", Sin.sin)
        , ("cos", Cos.cos)
        , ("tan", Tan.tan)
        , ("asin", Asin.asin)
        , ("acos", Acos.acos)
        , ("atan", Atan.atan)
        ]
