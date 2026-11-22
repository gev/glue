import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Text alignment enum object - represents all TextAlign values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TextAlign enum value
final textAlign = IrObject({
  'left': IrNativeValue(Value(TextAlign.left)),
  'right': IrNativeValue(Value(TextAlign.right)),
  'center': IrNativeValue(Value(TextAlign.center)),
  'justify': IrNativeValue(Value(TextAlign.justify)),
  'start': IrNativeValue(Value(TextAlign.start)),
  'end': IrNativeValue(Value(TextAlign.end)),
});
