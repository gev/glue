import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'list/car.dart';
import 'list/cdr.dart';

/// List module - list manipulation functions
/// Mirrors Haskell Glue.Lib.List exactly

/// The list module containing all list functions
/// Mirrors Haskell Glue.Lib.List.list exactly
final ModuleInfo list = nativeModule('ffi.list', [
  // Core list operations
  ('car', IrNative(NativeFunc(car))),
  ('cdr', IrNative(NativeFunc(cdr))),
]);
