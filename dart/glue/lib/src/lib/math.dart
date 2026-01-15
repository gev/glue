import 'package:glue/src/module.dart';
import 'math/arithmetic/arithmetic.dart';
import 'math/const.dart';
import 'math/logarithmic/logarithmic.dart';
import 'math/power/power.dart';
import 'math/trigonometric/trigonometric.dart';
import 'math/utility/utility.dart';

/// Math module - complete mathematical function library
/// Mirrors Haskell Glue.Lib.Math exactly

/// The math module containing all mathematical functions from all submodules
/// Mirrors Haskell Glue.Lib.Math.math exactly
final ModuleInfo math = nativeModule('ffi.math', [
  // Include all arithmetic functions (+, -, *, /, %)
  ...arithmetic.definitions,

  // Include all constants (pi, e)
  ...const_.definitions,

  // Include all logarithmic functions (lg, ln, log)
  ...logarithmic.definitions,

  // Include all power functions (exp, pow, sqrt)
  ...power.definitions,

  // Include all trigonometric functions (sin, cos, tan, asin, acos, atan)
  ...trigonometric.definitions,

  // Include all utility functions (abs, ceil, floor, max, min, round, trunc)
  ...utility.definitions,
]);
