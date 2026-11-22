import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// Brightness enum object
/// Represents all Brightness values as Glue object properties
final brightness = IrObject({
  'dark': IrNativeValue(Value(Brightness.dark)),
  'light': IrNativeValue(Value(Brightness.light)),
});
