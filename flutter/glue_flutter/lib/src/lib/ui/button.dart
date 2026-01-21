import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';

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

  final buttonWidget = ElevatedButton(
    onPressed: disabled ? null : onPressed,
    child: Text(label),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}

/// Extract string from Glue IR value
String? _extractString(dynamic value) => switch (value) {
  IrString(:final value) => value,
  String string => string,
  _ => null,
};

/// Extract bool from Glue IR value
bool? _extractBool(dynamic value) => switch (value) {
  IrBool(:final value) => value,
  bool boolean => boolean,
  _ => null,
};

/// Extract VoidCallback from Glue IR value
VoidCallback? _extractVoidCallback(dynamic value) => switch (value) {
  // TODO: Implement callback extraction from IrClosure
  _ => null,
};
