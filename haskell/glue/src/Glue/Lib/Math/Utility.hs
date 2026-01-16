module Glue.Lib.Math.Utility where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
import Glue.Lib.Math.Utility.Abs qualified as Abs
import Glue.Lib.Math.Utility.Ceil qualified as Ceil
import Glue.Lib.Math.Utility.Floor qualified as Floor

import Glue.Lib.Math.Utility.Max qualified as Max
import Glue.Lib.Math.Utility.Min qualified as Min
import Glue.Lib.Math.Utility.Round qualified as Round
import Glue.Lib.Math.Utility.Trunc qualified as Trunc
import Glue.Module (ModuleInfo, nativeModule)

utility :: ModuleInfo Eval
utility =
    nativeModule
        "ffi.math.utility"
        [ ("abs", NativeFunc Abs.abs)
        , ("floor", NativeFunc Floor.floor)
        , ("ceil", NativeFunc Ceil.ceil)
        , ("round", NativeFunc Round.round)
        , ("trunc", NativeFunc Trunc.trunc)
        , ("min", NativeFunc Min.min)
        , ("max", NativeFunc Max.max)
        ]
