import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'list/append.dart';
import 'list/butlast.dart';
import 'list/car.dart';
import 'list/cdr.dart';
import 'list/cons.dart';
import 'list/drop.dart';
import 'list/filter.dart';
import 'list/find.dart';
import 'list/flatten.dart';
import 'list/last.dart';
import 'list/length.dart';
import 'list/map.dart';
import 'list/member.dart';
import 'list/nth.dart';
import 'list/partition.dart';
import 'list/position.dart';
import 'list/remove.dart';
import 'list/reverse.dart';
import 'list/sort.dart';
import 'list/take.dart';
import 'list/zip.dart';

/// List module - list manipulation functions
/// Mirrors Haskell Glue.Lib.List exactly

/// The list module containing all list functions
/// Mirrors Haskell Glue.Lib.List.list exactly
final ModuleInfo list = nativeModule('ffi.list', [
  // Core list operations
  ('append', IrNativeFunc(append)),
  ('butlast', IrNativeFunc(butlast)),
  ('car', IrNativeFunc(car)),
  ('cdr', IrNativeFunc(cdr)),
  ('cons', IrNativeFunc(cons)),
  ('drop', IrNativeFunc(drop)),
  ('filter', IrNativeFunc(filter)),
  ('find', IrNativeFunc(find)),
  ('flatten', IrNativeFunc(flatten)),
  ('last', IrNativeFunc(last)),
  ('length', IrNativeFunc(length)),
  ('map', IrNativeFunc(map)),
  ('member', IrNativeFunc(member)),
  ('nth', IrNativeFunc(nth)),
  ('partition', IrNativeFunc(partition)),
  ('position', IrNativeFunc(position)),
  ('remove', IrNativeFunc(remove)),
  ('reverse', IrNativeFunc(reverse)),
  ('sort', IrNativeFunc(sort)),
  ('take', IrNativeFunc(take)),
  ('zip', IrNativeFunc(zip)),
]);
