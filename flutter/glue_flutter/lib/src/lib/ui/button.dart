import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object (label is in props)
Eval<Ir> buttonImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  // Extract properties using lazy wrapper
  final properties = Properties(props.properties.unlock);

  final buttonWidget = ElevatedButton(
    onPressed: properties.disabled == true ? null : properties.onTap,
    child: Text(properties.label ?? 'Button'),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}
