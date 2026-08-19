import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// BorderStyle object - represents available border styles as Glue object properties
/// Each property is a NativeValue wrapping the Flutter BorderStyle constant
final borderStyle = IrObject({
  'solid': IrNativeValue(Value(BorderStyle.solid)),
  'none': IrNativeValue(Value(BorderStyle.none)),
});
