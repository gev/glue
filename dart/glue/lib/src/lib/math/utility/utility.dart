import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'abs.dart';
import 'ceil.dart';
import 'floor.dart';
import 'max.dart';
import 'min.dart';
import 'round.dart';
import 'trunc.dart';

/// Utility module - mathematical utility functions
/// Mirrors Haskell Glue.Lib.Math.Utility exactly

/// The utility module containing mathematical utility functions
/// Mirrors Haskell Glue.Lib.Math.Utility.utility exactly
final ModuleInfo utility = nativeModule('ffi.math.utility', [
  // Absolute value
  ('abs', IrNative(NativeFunc(abs))),

  // Rounding functions (always return Integer)
  ('floor', IrNative(NativeFunc(floor))),
  ('ceil', IrNative(NativeFunc(ceil))),
  ('round', IrNative(NativeFunc(round))),
  ('trunc', IrNative(NativeFunc(trunc))),

  // Min/max functions (two arguments)
  ('min', IrNative(NativeFunc(min))),
  ('max', IrNative(NativeFunc(max))),
]);
