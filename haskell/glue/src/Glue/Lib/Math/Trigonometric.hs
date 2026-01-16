module Glue.Lib.Math.Trigonometric where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
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
        [ ("sin", NativeFunc Sin.sin)
        , ("cos", NativeFunc Cos.cos)
        , ("tan", NativeFunc Tan.tan)
        , ("asin", NativeFunc Asin.asin)
        , ("acos", NativeFunc Acos.acos)
        , ("atan", NativeFunc Atan.atan)
        ]
