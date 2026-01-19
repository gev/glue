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
        , ("eq", NativeFunc eq)
        , ("==", NativeFunc eq)
        , ("ne", NativeFunc ne)
        , ("!=", NativeFunc ne)
        , ("lt", NativeFunc lt)
        , ("<", NativeFunc lt)
        , ("le", NativeFunc le)
        , ("<=", NativeFunc le)
        , ("gt", NativeFunc gt)
        , (">", NativeFunc gt)
        , ("ge", NativeFunc ge)
        , (">=", NativeFunc ge)
        , ("not", NativeFunc not_)
        , ("!", NativeFunc not_)
        , ("if", Special if_)
        , ("when", Special when_)
        , ("while", Special while_)
        , ("until", Special until_)
        ]
