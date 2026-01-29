import 'package:flutter/widgets.dart';
import 'package:glue/src/ir.dart';

/// Orientation enum object
/// Represents all Orientation values as Glue object properties
final orientation = IrObject({
  'portrait': IrNativeValue(Value(Orientation.portrait)),
  'landscape': IrNativeValue(Value(Orientation.landscape)),
});
