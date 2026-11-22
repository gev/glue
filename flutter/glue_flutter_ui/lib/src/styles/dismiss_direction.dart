import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// DismissDirection enum object
/// Represents all DismissDirection values as Glue object properties
final dismissDirection = IrObject({
  'vertical': IrNativeValue(Value(DismissDirection.vertical)),
  'horizontal': IrNativeValue(Value(DismissDirection.horizontal)),
  'end-to-start': IrNativeValue(Value(DismissDirection.endToStart)),
  'start-to-end': IrNativeValue(Value(DismissDirection.startToEnd)),
});
