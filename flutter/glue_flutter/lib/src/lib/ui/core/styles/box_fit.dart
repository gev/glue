import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Box fit enum object - represents all BoxFit values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter BoxFit enum value
final boxFit = IrObject({
  'fill': IrNativeValue(Value(BoxFit.fill)),
  'contain': IrNativeValue(Value(BoxFit.contain)),
  'cover': IrNativeValue(Value(BoxFit.cover)),
  'fit-width': IrNativeValue(Value(BoxFit.fitWidth)),
  'fit-height': IrNativeValue(Value(BoxFit.fitHeight)),
  'none': IrNativeValue(Value(BoxFit.none)),
  'scale-down': IrNativeValue(Value(BoxFit.scaleDown)),
});
