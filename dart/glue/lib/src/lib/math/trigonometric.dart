import 'package:glue/src/module.dart';
import 'package:glue/src/lib/math/trigonometric/acos.dart';
import 'package:glue/src/lib/math/trigonometric/asin.dart';
import 'package:glue/src/lib/math/trigonometric/atan.dart';
import 'package:glue/src/lib/math/trigonometric/cos.dart';
import 'package:glue/src/lib/math/trigonometric/sin.dart';
import 'package:glue/src/lib/math/trigonometric/tan.dart';

/// Trigonometric module - trigonometric functions (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric exactly

/// The trigonometric module containing trigonometric functions
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.trigonometric exactly
final ModuleInfo trigonometricModule = nativeModule('ffi.math.trigonometric', [
  // Direct trigonometric functions
  ('sin', sin),
  ('cos', cos),
  ('tan', tan),

  // Inverse trigonometric functions
  ('asin', asin),
  ('acos', acos),
  ('atan', atan),
]);
