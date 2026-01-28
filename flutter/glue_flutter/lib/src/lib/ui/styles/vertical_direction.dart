import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Vertical direction enum object - represents all VerticalDirection values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter VerticalDirection enum value
final verticalDirection = IrObject({
  'up': IrNativeValue(Value(VerticalDirection.up)),
  'down': IrNativeValue(Value(VerticalDirection.down)),
});
