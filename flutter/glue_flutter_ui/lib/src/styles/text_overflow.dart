import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Text overflow enum object - represents all TextOverflow values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TextOverflow enum value
final textOverflow = IrObject({
  'clip': IrNativeValue(Value(TextOverflow.clip)),
  'fade': IrNativeValue(Value(TextOverflow.fade)),
  'ellipsis': IrNativeValue(Value(TextOverflow.ellipsis)),
  'visible': IrNativeValue(Value(TextOverflow.visible)),
});
