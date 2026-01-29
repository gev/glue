import 'package:flutter/widgets.dart';
import 'package:glue/src/ir.dart';

/// Main-axis alignment enum object - represents all MainAxisAlignment values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter MainAxisAlignment enum value
final mainAxisAlignment = IrObject({
  'start': IrNativeValue(Value(MainAxisAlignment.start)),
  'end': IrNativeValue(Value(MainAxisAlignment.end)),
  'center': IrNativeValue(Value(MainAxisAlignment.center)),
  'spaceBetween': IrNativeValue(Value(MainAxisAlignment.spaceBetween)),
  'spaceAround': IrNativeValue(Value(MainAxisAlignment.spaceAround)),
  'spaceEvenly': IrNativeValue(Value(MainAxisAlignment.spaceEvenly)),
});
