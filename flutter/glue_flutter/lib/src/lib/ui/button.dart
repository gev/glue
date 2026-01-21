import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object (label is in props)
Eval<Ir> buttonImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  // Extract values from Glue IR properties
  final properties = props.properties.unlock as Map<String, dynamic>;
  final label = extractString(properties['label']) ?? 'Button';
  final onPressed = extractVoidCallback(properties['on-tap']);
  final disabled = extractBool(properties['disabled']) ?? false;

  final buttonWidget = ElevatedButton(
    onPressed: disabled ? null : onPressed,
    child: Text(label),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}
