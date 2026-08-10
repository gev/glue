import 'dart:math' as math;

import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';

/// Math constants module
/// Mirrors Haskell Glue.Lib.Math.Const exactly

/// The const module containing mathematical constants
/// Mirrors Haskell Glue.Lib.Math.Const.const exactly
final ModuleInfo constModule = nativeModule('ffi.math.const', [
  ('e', IrFloat(math.e)),
  ('infinity', IrFloat(double.infinity)),
  ('minus-infinity', IrFloat(-double.infinity)),
  ('pi', IrFloat(math.pi)),
  ('phi', IrFloat((1 + math.sqrt(5)) / 2)),
  ('nan', IrFloat(double.nan)),
]);
