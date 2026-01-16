import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'add.dart';
import 'div.dart';
import 'mod.dart';
import 'mul.dart';
import 'sub.dart';

/// Arithmetic module - basic mathematical operations
/// Mirrors Haskell Glue.Lib.Math.Arithmetic exactly

/// The arithmetic module containing all basic math operations
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.arithmetic exactly
final ModuleInfo arithmetic = nativeModule('ffi.math.arithmetic', [
  // Addition
  ('+', IrNative(NativeFunc(add))),
  ('add', IrNative(NativeFunc(add))),

  // Subtraction
  ('-', IrNative(NativeFunc(sub))),
  ('sub', IrNative(NativeFunc(sub))),

  // Multiplication
  ('*', IrNative(NativeFunc(mul))),
  ('mul', IrNative(NativeFunc(mul))),

  // Division
  ('/', IrNative(NativeFunc(div))),
  ('div', IrNative(NativeFunc(div))),

  // Modulo
  ('%', IrNative(NativeFunc(mod))),
  ('mod', IrNative(NativeFunc(mod))),
]);
