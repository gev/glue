module Glue.Lib.Math.Trigonometric where

import Glue.Eval (Eval)
import Glue.IR (IR (..), Native (..))
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
        [ ("sin", Native (Func Sin.sin))
        , ("cos", Native (Func Cos.cos))
        , ("tan", Native (Func Tan.tan))
        , ("asin", Native (Func Asin.asin))
        , ("acos", Native (Func Acos.acos))
        , ("atan", Native (Func Atan.atan))
        ]
