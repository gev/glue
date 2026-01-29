import 'package:flutter/widgets.dart';
import 'package:glue/src/ir.dart';

/// Text direction enum object - represents all TextDirection values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TextDirection enum value
final textDirection = IrObject({
  'ltr': IrNativeValue(Value(TextDirection.ltr)),
  'rtl': IrNativeValue(Value(TextDirection.rtl)),
});
