import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Main-axis size enum object - represents all MainAxisSize values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter MainAxisSize enum value
final mainAxisSize = IrObject({
  'max': IrNativeValue(Value(MainAxisSize.max)),
  'min': IrNativeValue(Value(MainAxisSize.min)),
});
