import 'package:glue/src/lib/math/power/exp.dart';
import 'package:glue/src/lib/math/power/pow.dart';
import 'package:glue/src/lib/math/power/sqrt.dart';
import 'package:glue/src/module.dart';

/// Power module - exponential and power functions
/// Mirrors Haskell Glue.Lib.Math.Power exactly

/// The power module containing exponential and power operations
/// Mirrors Haskell Glue.Lib.Math.Power.power exactly
final ModuleInfo powerModule = nativeModule('ffi.math.power', [
  // Exponential function
  ('exp', exp),

  // Power function
  ('pow', pow),

  // Square root function
  ('sqrt', sqrt),
]);
