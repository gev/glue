module Glue.Lib.Math.Const where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Module (ModuleInfo, nativeModule)

const :: ModuleInfo Eval
const =
    nativeModule
        "ffi.math.const"
        [ ("pi", Float pi)
        , ("e", Float (exp 1))
        ]
