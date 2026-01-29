import 'package:flutter/widgets.dart';
import 'package:glue/src/ir.dart';

/// DismissDirection enum object
/// Represents all DismissDirection values as Glue object properties
final dismissDirection = IrObject({
  'vertical': IrNativeValue(Value(DismissDirection.vertical)),
  'horizontal': IrNativeValue(Value(DismissDirection.horizontal)),
  'endToStart': IrNativeValue(Value(DismissDirection.endToStart)),
  'startToEnd': IrNativeValue(Value(DismissDirection.startToEnd)),
});
