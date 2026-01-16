import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'exp.dart';
import 'pow.dart';
import 'sqrt.dart';

/// Power module - exponential and power functions
/// Mirrors Haskell Glue.Lib.Math.Power exactly

/// The power module containing exponential and power operations
/// Mirrors Haskell Glue.Lib.Math.Power.power exactly
final ModuleInfo power = nativeModule('ffi.math.power', [
  // Exponential function
  ('exp', IrNativeFunc(exp)),

  // Power function
  ('pow', IrNativeFunc(pow)),

  // Square root function
  ('sqrt', IrNativeFunc(sqrt)),
]);
