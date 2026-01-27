import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Cross-axis alignment enum object - represents all CrossAxisAlignment values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter CrossAxisAlignment enum value
final crossAxisAlignment = IrObject({
  'start': IrNativeValue(Value(CrossAxisAlignment.start)),
  'end': IrNativeValue(Value(CrossAxisAlignment.end)),
  'center': IrNativeValue(Value(CrossAxisAlignment.center)),
  'stretch': IrNativeValue(Value(CrossAxisAlignment.stretch)),
  'baseline': IrNativeValue(Value(CrossAxisAlignment.baseline)),
});
