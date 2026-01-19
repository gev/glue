module Glue.Lib.Bool where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Lib.Bool.Eq (eq)
import Glue.Lib.Bool.Ge (ge)
import Glue.Lib.Bool.Gt (gt)
import Glue.Lib.Bool.If (if_)
import Glue.Lib.Bool.Le (le)
import Glue.Lib.Bool.Lt (lt)
import Glue.Lib.Bool.Ne (ne)
import Glue.Lib.Bool.Not (not_)
import Glue.Lib.Bool.Until (until_)
import Glue.Lib.Bool.When (when_)
import Glue.Lib.Bool.While (while_)
import Glue.Module (ModuleInfo, nativeModule)

bool :: ModuleInfo Eval
bool =
    nativeModule
        "ffi.bool"
        [ ("true", Bool True)
        , ("false", Bool False)
        , ("==", eq)
        , ("ne", ne)
        , ("!=", ne)
        , ("lt", lt)
        , ("<", lt)
        , ("le", le)
        , ("<=", le)
        , ("gt", gt)
        , (">", gt)
        , ("ge", ge)
        , (">=", ge)
        , ("not", not_)
        , ("!", not_)
        , ("if", if_)
        , ("when", when_)
        , ("while", while_)
        , ("until", until_)
        ]
