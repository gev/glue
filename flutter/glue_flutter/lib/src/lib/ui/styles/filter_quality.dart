import 'dart:ui';
import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Filter quality enum object - represents all FilterQuality values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FilterQuality enum value
final filterQuality = IrObject({
  'none': IrNativeValue(Value(FilterQuality.none)),
  'low': IrNativeValue(Value(FilterQuality.low)),
  'medium': IrNativeValue(Value(FilterQuality.medium)),
  'high': IrNativeValue(Value(FilterQuality.high)),
});
