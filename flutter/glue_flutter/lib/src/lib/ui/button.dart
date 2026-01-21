import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_button.dart';

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
  final label = _extractString(properties['label']) ?? 'Button';
  final onPressed = _extractVoidCallback(properties['on-tap']);
  final disabled = _extractBool(properties['disabled']) ?? false;

  final buttonWidget = GlueButton(
    label: label,
    onPressed: onPressed,
    disabled: disabled,
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}

/// Extract string from Glue IR value
String? _extractString(dynamic value) {
  if (value == null) return null;
  if (value is IrString) return value.value;
  if (value is String) return value;
  return null;
}

/// Extract bool from Glue IR value
bool? _extractBool(dynamic value) {
  if (value == null) return null;
  if (value is IrBool) return value.value;
  if (value is bool) return value;
  return null;
}

/// Extract VoidCallback from Glue IR value
VoidCallback? _extractVoidCallback(dynamic value) {
  if (value == null) return null;

  // TODO: Implement callback extraction from IrClosure
  // For now, return null
  return null;
}
