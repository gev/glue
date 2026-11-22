import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Flutter logo style enum object - represents all FlutterLogoStyle values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter FlutterLogoStyle enum value
final flutterLogoStyle = IrObject({
  'mark-only': IrNativeValue(Value(FlutterLogoStyle.markOnly)),
  'horizontal': IrNativeValue(Value(FlutterLogoStyle.horizontal)),
  'stacked': IrNativeValue(Value(FlutterLogoStyle.stacked)),
});
