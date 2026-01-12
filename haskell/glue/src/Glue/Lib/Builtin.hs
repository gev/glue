module Glue.Lib.Builtin where

import Glue.Eval (Eval)
import Glue.IR (IR (..), Native (..))
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
        [ ("def", Native (Special def))
        , ("set", Native (Special set))
        , ("lambda", Native (Special lambda))
        , ("\\", Native (Special lambda))
        , ("let", Native (Special let'))
        , ("import", Native (Special importForm))
        , ("error", Native (Special errorFunc))
        , ("try", Native (Special tryFunc))
        ]
