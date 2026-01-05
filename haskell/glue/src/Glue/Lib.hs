module Glue.Lib where

import Glue.Env qualified as E
import Glue.Eval (Eval)
import Glue.IR (Frame)
import Glue.Lib.Bool qualified as Bool
import Glue.Lib.Builtin qualified as Builtin
import Glue.Lib.IO qualified as IO
import Glue.Lib.List qualified as List
import Glue.Lib.Math qualified as Math
import Glue.Lib.Math.Arithmetic qualified as Arithmetic

lib :: Frame Eval
lib =
    E.unionFramesList
        [ Builtin.builtin
        , Bool.bool
        , IO.io
        , List.list
        , Arithmetic.arithmetic
        , Math.math
        ]
