import 'package:glue/src/lib/math/utility/abs.dart';
import 'package:glue/src/lib/math/utility/ceil.dart';
import 'package:glue/src/lib/math/utility/floor.dart';
import 'package:glue/src/lib/math/utility/max.dart';
import 'package:glue/src/lib/math/utility/min.dart';
import 'package:glue/src/lib/math/utility/round.dart';
import 'package:glue/src/lib/math/utility/trunc.dart';
import 'package:glue/src/module.dart';

/// Utility module - mathematical utility functions
/// Mirrors Haskell Glue.Lib.Math.Utility exactly

/// The utility module containing mathematical utility functions
/// Mirrors Haskell Glue.Lib.Math.Utility.utility exactly
final ModuleInfo utilityModule = nativeModule('ffi.math.utility', [
  // Absolute value
  ('abs', abs),

  // Rounding functions (always return Integer)
  ('floor', floor),
  ('ceil', ceil),
  ('round', round),
  ('trunc', trunc),

  // Min/max functions (two arguments)
  ('min', min),
  ('max', max),
]);
