module Glue.Lib.Math.Logarithmic where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Lib.Math.Logarithmic.Lg qualified as Lg
import Glue.Lib.Math.Logarithmic.Ln qualified as Ln
import Glue.Lib.Math.Logarithmic.Log qualified as Log
import Glue.Module (ModuleInfo, nativeModule)

logarithmic :: ModuleInfo Eval
logarithmic =
    nativeModule
        "ffi.math.logarithmic"
        [ ("log", NativeFunc Log.log)
        , ("ln", NativeFunc Ln.ln)
        , ("lg", NativeFunc Lg.lg)
        ]
