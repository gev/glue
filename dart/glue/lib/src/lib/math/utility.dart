import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/lib/math/utility/abs.dart';
import 'package:glue/src/lib/math/utility/ceil.dart';
import 'package:glue/src/lib/math/utility/floor.dart';
import 'package:glue/src/lib/math/utility/max.dart';
import 'package:glue/src/lib/math/utility/min.dart';
import 'package:glue/src/lib/math/utility/round.dart';
import 'package:glue/src/lib/math/utility/trunc.dart';

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
