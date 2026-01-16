module Glue.Lib.Builtin where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
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
        [ ("def", Special def)
        , ("set", Special set)
        , ("lambda", Special lambda)
        , ("\\", Special lambda)
        , ("let", Special let')
        , ("import", Special importForm)
        , ("error", Special errorFunc)
        , ("try", Special tryFunc)
        ]
