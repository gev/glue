import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Axis enum object - represents all Axis values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Axis enum value
final alignment = IrObject({
  'bottom-center': IrNativeValue(Value(Alignment.bottomCenter)),
  'bottom-left': IrNativeValue(Value(Alignment.bottomLeft)),
  'bottom-right': IrNativeValue(Value(Alignment.bottomRight)),
  'center': IrNativeValue(Value(Alignment.center)),
  'center-left': IrNativeValue(Value(Alignment.centerLeft)),
  'center-right': IrNativeValue(Value(Alignment.centerRight)),
  'top-center': IrNativeValue(Value(Alignment.topCenter)),
  'top-left': IrNativeValue(Value(Alignment.topLeft)),
  'top-right': IrNativeValue(Value(Alignment.topRight)),
});
