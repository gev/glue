module Glue.Lib.Bool where

import Glue.Eval (Eval)
import Glue.IR (IR (..), Native (..))
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
        , ("eq", Native (Func eq))
        , ("==", Native (Func eq))
        , ("ne", Native (Func ne))
        , ("!=", Native (Func ne))
        , ("lt", Native (Func lt))
        , ("<", Native (Func lt))
        , ("le", Native (Func le))
        , ("<=", Native (Func le))
        , ("gt", Native (Func gt))
        , (">", Native (Func gt))
        , ("ge", Native (Func ge))
        , (">=", Native (Func ge))
        , ("not", Native (Func not_))
        , ("!", Native (Func not_))
        , ("if", Native (Special if_))
        , ("when", Native (Special when_))
        , ("while", Native (Special while_))
        , ("until", Native (Special until_))
        ]
