import 'dart:math' as math;

import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';

/// Math constants module
/// Mirrors Haskell Glue.Lib.Math.Const exactly

/// The const module containing mathematical constants
/// Mirrors Haskell Glue.Lib.Math.Const.const exactly
final ModuleInfo const_ = nativeModule('ffi.math.const', [
  // Mathematical constants
  ('pi', IrFloat(math.pi)),
  ('e', IrFloat(math.e)),
]);
