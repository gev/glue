import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Wrap cross alignment enum object - represents all WrapCrossAlignment values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter WrapCrossAlignment enum value
final wrapCrossAlignment = IrObject({
  'start': IrNativeValue(Value(WrapCrossAlignment.start)),
  'end': IrNativeValue(Value(WrapCrossAlignment.end)),
  'center': IrNativeValue(Value(WrapCrossAlignment.center)),
});
