module Glue.Lib.List where

import Glue.Eval (Eval)
import Glue.Lib.List.Append (append)
import Glue.Lib.List.Butlast (butlast)
import Glue.Lib.List.Concat qualified as Concat
import Glue.Lib.List.Drop qualified as Drop
import Glue.Lib.List.Filter qualified as Filter
import Glue.Lib.List.Find qualified as Find
import Glue.Lib.List.Flatten qualified as Flatten
import Glue.Lib.List.Foldl qualified as Foldl
import Glue.Lib.List.Foldr qualified as Foldr
import Glue.Lib.List.Head qualified as Head
import Glue.Lib.List.Last qualified as Last
import Glue.Lib.List.Length qualified as Length
import Glue.Lib.List.Map qualified as Map
import Glue.Lib.List.Member (member)
import Glue.Lib.List.Partition qualified as Partition
import Glue.Lib.List.Position qualified as Position
import Glue.Lib.List.Prepend (prepend)
import Glue.Lib.List.Remove (remove)
import Glue.Lib.List.Reverse qualified as Reverse
import Glue.Lib.List.Sort qualified as Sort
import Glue.Lib.List.Tail qualified as Tail
import Glue.Lib.List.Take qualified as Take
import Glue.Lib.List.Zip qualified as Zip
import Glue.Module (ModuleInfo, nativeModule)

list :: ModuleInfo Eval
list =
    nativeModule
        "ffi.list"
        [ ("append", append)
        , ("but-last", butlast)
        , ("concat", Concat.concat)
        , ("head", Head.head)
        , ("drop", Drop.drop)
        , ("filter", Filter.filter)
        , ("find", Find.find)
        , ("flatten", Flatten.flatten)
        , ("foldl", Foldl.foldl)
        , ("foldr", Foldr.foldr)
        , ("last", Last.last)
        , ("length", Length.length)
        , ("map", Map.map)
        , ("member", member)
        , ("partition", Partition.partition)
        , ("prepend", prepend)
        , ("position", Position.position)
        , ("remove", remove)
        , ("reverse", Reverse.reverse)
        , ("sort", Sort.sort)
        , ("tail", Tail.tail)
        , ("take", Take.take)
        , ("zip", Zip.zip)
        ]
