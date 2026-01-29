import 'dart:ui';

import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Filter quality enum object - represents all FilterQuality values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FilterQuality enum value
final filterQuality = IrObject({
  'none': IrNativeValue(Value(FilterQuality.none)),
  'low': IrNativeValue(Value(FilterQuality.low)),
  'medium': IrNativeValue(Value(FilterQuality.medium)),
  'high': IrNativeValue(Value(FilterQuality.high)),
});
