import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/lib/math/arithmetic/add.dart';
import 'package:glue/src/lib/math/arithmetic/div.dart';
import 'package:glue/src/lib/math/arithmetic/mod.dart';
import 'package:glue/src/lib/math/arithmetic/mul.dart';
import 'package:glue/src/lib/math/arithmetic/sub.dart';

/// Arithmetic module - basic mathematical operations
/// Mirrors Haskell Glue.Lib.Math.Arithmetic exactly

/// The arithmetic module containing all basic math operations
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.arithmetic exactly
final ModuleInfo arithmetic = nativeModule('ffi.math.arithmetic', [
  // Addition
  ('+', IrNativeFunc(add)),
  ('add', IrNativeFunc(add)),

  // Subtraction
  ('-', IrNativeFunc(sub)),
  ('sub', IrNativeFunc(sub)),

  // Multiplication
  ('*', IrNativeFunc(mul)),
  ('mul', IrNativeFunc(mul)),

  // Division
  ('/', IrNativeFunc(div)),
  ('div', IrNativeFunc(div)),

  // Modulo
  ('%', IrNativeFunc(mod)),
  ('mod', IrNativeFunc(mod)),
]);
