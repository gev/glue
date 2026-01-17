module Glue.Lib.List where

import Glue.Eval (Eval)
import Glue.IR (IR (..))
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
        [ ("append", NativeFunc append)
        , ("butlast", NativeFunc butlast)
        , ("car", NativeFunc car)
        , ("cdr", NativeFunc cdr)
        , ("cons", NativeFunc cons)
        , ("drop", NativeFunc Drop.drop)
        , ("filter", NativeFunc Filter.filter)
        , ("find", NativeFunc Find.find)
        , ("flatten", NativeFunc Flatten.flatten)
        , ("last", NativeFunc Last.last)
        , ("length", NativeFunc Length.length)
        , ("map", NativeFunc Map.map)
        , ("member", NativeFunc member)
        , ("nth", NativeFunc nth)
        , ("partition", NativeFunc Partition.partition)
        , ("position", NativeFunc Position.position)
        , ("remove", NativeFunc remove)
        , ("reverse", NativeFunc Reverse.reverse)
        , ("sort", NativeFunc Sort.sort)
        , ("take", NativeFunc Take.take)
        , ("zip", NativeFunc Zip.zip)
        ]
