import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'lg.dart';
import 'ln.dart';
import 'log.dart';

/// Logarithmic module - logarithm functions with various bases
/// Mirrors Haskell Glue.Lib.Math.Logarithmic exactly

/// The logarithmic module containing logarithm functions
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.logarithmic exactly
final ModuleInfo logarithmic = nativeModule('ffi.math.logarithmic', [
  // Logarithm with arbitrary base (value, base)
  ('log', IrNativeFunc(log)),

  // Natural logarithm (base e)
  ('ln', IrNativeFunc(ln)),

  // Common logarithm (base 10)
  ('lg', IrNativeFunc(lg)),
]);
