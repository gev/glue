module Glue.Lib.List where

import Glue.Eval (Eval)
import Glue.IR (IR (..), Native (..))
import Glue.Lib.List.Append (append)
import Glue.Lib.List.Butlast (butlast)
import Glue.Lib.List.Car (car)
import Glue.Lib.List.Cdr (cdr)
import Glue.Lib.List.Cons (cons)
import Glue.Lib.List.Drop qualified as Drop
import Glue.Lib.List.Filter qualified as Filter
import Glue.Lib.List.Find qualified as Find
import Glue.Lib.List.Flatten qualified as Flatten
import Glue.Lib.List.Last qualified as Last
import Glue.Lib.List.Length qualified as Length
import Glue.Lib.List.Map qualified as Map
import Glue.Lib.List.Member (member)
import Glue.Lib.List.Nth (nth)
import Glue.Lib.List.Partition qualified as Partition
import Glue.Lib.List.Position qualified as Position
import Glue.Lib.List.Remove (remove)
import Glue.Lib.List.Reverse qualified as Reverse
import Glue.Lib.List.Sort qualified as Sort
import Glue.Lib.List.Take qualified as Take
import Glue.Lib.List.Zip qualified as Zip
import Glue.Module (ModuleInfo, nativeModule)

list :: ModuleInfo Eval
list =
    nativeModule
        "ffi.list"
        [ ("append", Native (Func append))
        , ("butlast", Native (Func butlast))
        , ("car", Native (Func car))
        , ("cdr", Native (Func cdr))
        , ("cons", Native (Func cons))
        , ("drop", Native (Func Drop.drop))
        , ("filter", Native (Func Filter.filter))
        , ("find", Native (Func Find.find))
        , ("flatten", Native (Func Flatten.flatten))
        , ("last", Native (Func Last.last))
        , ("length", Native (Func Length.length))
        , ("map", Native (Func Map.map))
        , ("member", Native (Func member))
        , ("nth", Native (Func nth))
        , ("partition", Native (Func Partition.partition))
        , ("position", Native (Func Position.position))
        , ("remove", Native (Func remove))
        , ("reverse", Native (Func Reverse.reverse))
        , ("sort", Native (Func Sort.sort))
        , ("take", Native (Func Take.take))
        , ("zip", Native (Func Zip.zip))
        ]
