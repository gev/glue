module Glue.Lib.Module where

import Glue.Eval (Eval)
import Glue.Lib.Module.Import (importForm)
import Glue.Module (ModuleInfo, nativeModule)

moduleModule :: ModuleInfo Eval
moduleModule =
    nativeModule
        "ffi.module"
        [ ("import", importForm)
        ]
