module Glue.Lib.Builtin where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame, IR (..), Native (..))
import Glue.Lib.Builtin.Def (def)
import Glue.Lib.Builtin.Error (errorFunc)
import Glue.Lib.Builtin.Import (importForm)
import Glue.Lib.Builtin.Lambda (lambda)
import Glue.Lib.Builtin.Set (set)

builtin :: Frame Eval
builtin =
    E.frameFromList
        [ ("def", Native (Special def))
        , ("set", Native (Special set))
        , ("lambda", Native (Special lambda))
        , ("\\", Native (Special lambda))
        , ("import", Native (Special importForm))
        , ("error", Native (Special errorFunc))
        ]
