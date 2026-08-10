module Glue.Lib.Math.Const where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Module (ModuleInfo, nativeModule)

const :: ModuleInfo Eval
const =
    nativeModule
        "ffi.math.const"
        [ ("e", Float (exp 1))
        , ("infinity", Float (1 / 0))
        , ("minus-infinity", Float ((-1) / 0))
        , ("pi", Float pi)
        , ("phi", Float ((1 + sqrt 5) / 2))
        ]
