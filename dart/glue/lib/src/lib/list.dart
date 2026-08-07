import 'package:glue/src/lib/list/append.dart';
import 'package:glue/src/lib/list/butlast.dart';
import 'package:glue/src/lib/list/concat.dart';
import 'package:glue/src/lib/list/drop.dart';
import 'package:glue/src/lib/list/filter.dart';
import 'package:glue/src/lib/list/find.dart';
import 'package:glue/src/lib/list/flatten.dart';
import 'package:glue/src/lib/list/foldl.dart';
import 'package:glue/src/lib/list/foldr.dart';
import 'package:glue/src/lib/list/head.dart';
import 'package:glue/src/lib/list/last.dart';
import 'package:glue/src/lib/list/length.dart';
import 'package:glue/src/lib/list/map.dart';
import 'package:glue/src/lib/list/member.dart';
import 'package:glue/src/lib/list/partition.dart';
import 'package:glue/src/lib/list/position.dart';
import 'package:glue/src/lib/list/prepend.dart';
import 'package:glue/src/lib/list/remove.dart';
import 'package:glue/src/lib/list/reverse.dart';
import 'package:glue/src/lib/list/sort.dart';
import 'package:glue/src/lib/list/tail.dart';
import 'package:glue/src/lib/list/take.dart';
import 'package:glue/src/lib/list/zip.dart';
import 'package:glue/src/module.dart';

/// List module - list manipulation functions
/// Mirrors Haskell Glue.Lib.List exactly

/// The list module containing all list functions
/// Mirrors Haskell Glue.Lib.List.list exactly
final ModuleInfo listModule = nativeModule('ffi.list', [
  // Core list operations
  ('append', append),
  ('butlast', butlast),
  ('concat', concat),
  ('drop', drop),
  ('filter', filter),
  ('find', find),
  ('flatten', flatten),
  ('foldl', foldl),
  ('foldr', foldr),
  ('head', head),
  ('last', last),
  ('length', length),
  ('map', map),
  ('member', member),
  ('partition', partition),
  ('position', position),
  ('prepend', prepend),
  ('remove', remove),
  ('reverse', reverse),
  ('sort', sort),
  ('tail', tail),
  ('take', take),
  ('zip', zip),
]);
