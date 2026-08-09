import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// TileMode enum object - represents all TileMode values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TileMode enum value
final tileMode = IrObject({
  'clamp': IrNativeValue(Value(TileMode.clamp)),
  'repeated': IrNativeValue(Value(TileMode.repeated)),
  'mirror': IrNativeValue(Value(TileMode.mirror)),
  'decal': IrNativeValue(Value(TileMode.decal)),
});
