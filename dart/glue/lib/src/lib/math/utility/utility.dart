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
  ('abs', IrNativeFunc(abs)),

  // Rounding functions (always return Integer)
  ('floor', IrNativeFunc(floor)),
  ('ceil', IrNativeFunc(ceil)),
  ('round', IrNativeFunc(round)),
  ('trunc', IrNativeFunc(trunc)),

  // Min/max functions (two arguments)
  ('min', IrNativeFunc(min)),
  ('max', IrNativeFunc(max)),
]);
