module Glue.Lib.Builtin where

import Glue.Eval (Eval)
import Glue.Lib.Builtin.Def (def)
import Glue.Lib.Builtin.Error (errorFunc)
import Glue.Lib.Builtin.Import (importForm)
import Glue.Lib.Builtin.Lambda (lambda)
import Glue.Lib.Builtin.Let (let')
import Glue.Lib.Builtin.Set (set)
import Glue.Lib.Builtin.Try (tryFunc)
import Glue.Module (ModuleInfo, nativeModule)

builtin :: ModuleInfo Eval
builtin =
    nativeModule
        "ffi.builtin"
        [ ("def", def)
        , ("set", set)
        , ("lambda", lambda)
        , ("\\", lambda)
        , ("let", let')
        , ("import", importForm)
        , ("error", errorFunc)
        , ("try", tryFunc)
        ]
