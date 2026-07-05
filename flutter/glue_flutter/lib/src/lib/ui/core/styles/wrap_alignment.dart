import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Wrap alignment enum object - represents all WrapAlignment values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter WrapAlignment enum value
final wrapAlignment = IrObject({
  'start': IrNativeValue(Value(WrapAlignment.start)),
  'end': IrNativeValue(Value(WrapAlignment.end)),
  'center': IrNativeValue(Value(WrapAlignment.center)),
  'space-between': IrNativeValue(Value(WrapAlignment.spaceBetween)),
  'space-around': IrNativeValue(Value(WrapAlignment.spaceAround)),
  'space-evenly': IrNativeValue(Value(WrapAlignment.spaceEvenly)),
});
