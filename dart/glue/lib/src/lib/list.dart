import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'list/append.dart';
import 'list/butlast.dart';
import 'list/car.dart';
import 'list/cdr.dart';
import 'list/cons.dart';
import 'list/drop.dart';
import 'list/filter.dart';
import 'list/last.dart';
import 'list/length.dart';
import 'list/map.dart';
import 'list/nth.dart';
import 'list/reverse.dart';
import 'list/sort.dart';
import 'list/take.dart';

/// List module - list manipulation functions
/// Mirrors Haskell Glue.Lib.List exactly

/// The list module containing all list functions
/// Mirrors Haskell Glue.Lib.List.list exactly
final ModuleInfo list = nativeModule('ffi.list', [
  // Core list operations
  ('append', IrNative(NativeFunc(append))),
  ('butlast', IrNative(NativeFunc(butlast))),
  ('car', IrNative(NativeFunc(car))),
  ('cdr', IrNative(NativeFunc(cdr))),
  ('cons', IrNative(NativeFunc(cons))),
  ('drop', IrNative(NativeFunc(drop))),
  ('filter', IrNative(NativeFunc(filter))),
  ('last', IrNative(NativeFunc(last))),
  ('length', IrNative(NativeFunc(length))),
  ('map', IrNative(NativeFunc(map))),
  ('nth', IrNative(NativeFunc(nth))),
  ('reverse', IrNative(NativeFunc(reverse))),
  ('sort', IrNative(NativeFunc(sort))),
  ('take', IrNative(NativeFunc(take))),
]);
