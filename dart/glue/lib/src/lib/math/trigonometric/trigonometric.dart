import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'acos.dart';
import 'asin.dart';
import 'atan.dart';
import 'cos.dart';
import 'sin.dart';
import 'tan.dart';

/// Trigonometric module - trigonometric functions (radians)
/// Mirrors Haskell Glue.Lib.Math.Trigonometric exactly

/// The trigonometric module containing trigonometric functions
/// Mirrors Haskell Glue.Lib.Math.Trigonometric.trigonometric exactly
final ModuleInfo trigonometric = nativeModule('ffi.math.trigonometric', [
  // Direct trigonometric functions
  ('sin', IrNativeFunc(sin)),
  ('cos', IrNativeFunc(cos)),
  ('tan', IrNativeFunc(tan)),

  // Inverse trigonometric functions
  ('asin', IrNativeFunc(asin)),
  ('acos', IrNativeFunc(acos)),
  ('atan', IrNativeFunc(atan)),
]);
