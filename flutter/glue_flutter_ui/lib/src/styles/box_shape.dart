import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// BoxShape enum object
/// Represents all BoxShape values as Glue object properties
final boxShape = IrObject({
  'rectangle': IrNativeValue(Value(BoxShape.rectangle)),
  'circle': IrNativeValue(Value(BoxShape.circle)),
});
