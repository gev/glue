import 'package:glue/src/module.dart';
import 'package:glue/src/lib/math/logarithmic/lg.dart';
import 'package:glue/src/lib/math/logarithmic/ln.dart';
import 'package:glue/src/lib/math/logarithmic/log.dart';

/// Logarithmic module - logarithm functions with various bases
/// Mirrors Haskell Glue.Lib.Math.Logarithmic exactly

/// The logarithmic module containing logarithm functions
/// Mirrors Haskell Glue.Lib.Math.Logarithmic.logarithmic exactly
final ModuleInfo logarithmicModule = nativeModule('ffi.math.logarithmic', [
  // Logarithm with arbitrary base (value, base)
  ('log', log),

  // Natural logarithm (base e)
  ('ln', ln),

  // Common logarithm (base 10)
  ('lg', lg),
]);
