import 'package:glue/src/module.dart';
import 'package:glue/src/lib/list/append.dart';
import 'package:glue/src/lib/list/butlast.dart';
import 'package:glue/src/lib/list/car.dart';
import 'package:glue/src/lib/list/cdr.dart';
import 'package:glue/src/lib/list/cons.dart';
import 'package:glue/src/lib/list/drop.dart';
import 'package:glue/src/lib/list/filter.dart';
import 'package:glue/src/lib/list/find.dart';
import 'package:glue/src/lib/list/flatten.dart';
import 'package:glue/src/lib/list/last.dart';
import 'package:glue/src/lib/list/length.dart';
import 'package:glue/src/lib/list/map.dart';
import 'package:glue/src/lib/list/member.dart';
import 'package:glue/src/lib/list/nth.dart';
import 'package:glue/src/lib/list/partition.dart';
import 'package:glue/src/lib/list/position.dart';
import 'package:glue/src/lib/list/remove.dart';
import 'package:glue/src/lib/list/reverse.dart';
import 'package:glue/src/lib/list/sort.dart';
import 'package:glue/src/lib/list/take.dart';
import 'package:glue/src/lib/list/zip.dart';

/// List module - list manipulation functions
/// Mirrors Haskell Glue.Lib.List exactly

/// The list module containing all list functions
/// Mirrors Haskell Glue.Lib.List.list exactly
final ModuleInfo listModule = nativeModule('ffi.list', [
  // Core list operations
  ('append', append),
  ('butlast', butlast),
  ('car', car),
  ('cdr', cdr),
  ('cons', cons),
  ('drop', drop),
  ('filter', filter),
  ('find', find),
  ('flatten', flatten),
  ('last', last),
  ('length', length),
  ('map', map),
  ('member', member),
  ('nth', nth),
  ('partition', partition),
  ('position', position),
  ('remove', remove),
  ('reverse', reverse),
  ('sort', sort),
  ('take', take),
  ('zip', zip),
]);
