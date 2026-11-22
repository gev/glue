import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

final stackFit = IrObject({
  'lose': IrNativeValue(Value(StackFit.loose)),
  'expand': IrNativeValue(Value(StackFit.expand)),
  'passthrough': IrNativeValue(Value(StackFit.passthrough)),
});
