import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Axis enum object - represents all Axis values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter Axis enum value
final alignment = IrObject({
  'top-left': IrNativeValue(Value(Alignment.topLeft)),
  'top-center': IrNativeValue(Value(Alignment.topCenter)),
  'top-right': IrNativeValue(Value(Alignment.topRight)),
  'center-left': IrNativeValue(Value(Alignment.centerLeft)),
  'center': IrNativeValue(Value(Alignment.center)),
  'center-right': IrNativeValue(Value(Alignment.centerRight)),
  'bottom-left': IrNativeValue(Value(Alignment.bottomLeft)),
  'bottom-center': IrNativeValue(Value(Alignment.bottomCenter)),
  'bottom-right': IrNativeValue(Value(Alignment.bottomRight)),
});
