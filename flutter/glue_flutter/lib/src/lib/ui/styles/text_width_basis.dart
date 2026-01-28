import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Text width basis enum object - represents all TextWidthBasis values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TextWidthBasis enum value
final textWidthBasis = IrObject({
  'parent': IrNativeValue(Value(TextWidthBasis.parent)),
  'longestLine': IrNativeValue(Value(TextWidthBasis.longestLine)),
});
