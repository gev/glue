import 'package:glue/src/lib/math/arithmetic/add.dart';
import 'package:glue/src/lib/math/arithmetic/div.dart';
import 'package:glue/src/lib/math/arithmetic/mod.dart';
import 'package:glue/src/lib/math/arithmetic/mul.dart';
import 'package:glue/src/lib/math/arithmetic/sub.dart';
import 'package:glue/src/module.dart';

/// Arithmetic module - basic mathematical operations
/// Mirrors Haskell Glue.Lib.Math.Arithmetic exactly

/// The arithmetic module containing all basic math operations
/// Mirrors Haskell Glue.Lib.Math.Arithmetic.arithmetic exactly
final ModuleInfo arithmeticModule = nativeModule('ffi.math.arithmetic', [
  // Addition
  ('+', add),
  ('add', add),

  // Subtraction
  ('-', sub),
  ('sub', sub),

  // Multiplication
  ('*', mul),
  ('mul', mul),

  // Division
  ('/', div),
  ('div', div),

  // Modulo
  ('%', mod),
  ('mod', mod),
]);
