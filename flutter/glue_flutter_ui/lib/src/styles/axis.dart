import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Axis enum object - represents all Axis values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Axis enum value
final axis = IrObject({
  'horizontal': IrNativeValue(Value(Axis.horizontal)),
  'vertical': IrNativeValue(Value(Axis.vertical)),
});
