import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Image repeat enum object - represents all ImageRepeat values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter ImageRepeat enum value
final imageRepeat = IrObject({
  'noRepeat': IrNativeValue(Value(ImageRepeat.noRepeat)),
  'repeat': IrNativeValue(Value(ImageRepeat.repeat)),
  'repeatX': IrNativeValue(Value(ImageRepeat.repeatX)),
  'repeatY': IrNativeValue(Value(ImageRepeat.repeatY)),
});
