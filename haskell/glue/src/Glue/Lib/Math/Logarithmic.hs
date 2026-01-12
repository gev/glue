module Glue.Lib.Math.Logarithmic where

import Glue.Eval (Eval)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.Math.Logarithmic.Lg qualified as Lg
import Glue.Lib.Math.Logarithmic.Ln qualified as Ln
import Glue.Lib.Math.Logarithmic.Log qualified as Log
import Glue.Module (ModuleInfo, nativeModule)

logarithmic :: ModuleInfo Eval
logarithmic =
    nativeModule
        "ffi.math.logarithmic"
        [ ("log", Native (Func Log.log))
        , ("ln", Native (Func Ln.ln))
        , ("lg", Native (Func Lg.lg))
        ]
