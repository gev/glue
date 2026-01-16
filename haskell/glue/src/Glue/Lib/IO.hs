module Glue.Lib.IO where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Lib.IO.Print (printFunc, println)
import Glue.Lib.IO.Read (readLine)
import Glue.Module (ModuleInfo, nativeModule)

io :: ModuleInfo Eval
io =
    nativeModule
        "ffi.io"
        [ ("print", NativeFunc printFunc)
        , ("println", NativeFunc println)
        , ("read-line", NativeFunc readLine)
        ]
