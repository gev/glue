import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

final blurStyle = IrObject({
  'normal': IrNativeValue(Value(BlurStyle.normal)),
  'inner': IrNativeValue(Value(BlurStyle.inner)),
  'outer': IrNativeValue(Value(BlurStyle.outer)),
  'solid': IrNativeValue(Value(BlurStyle.solid)),
});
