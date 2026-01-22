import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
/// Expects keyword arguments: :label, :on-tap, :disabled, etc.
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object with keyword arguments
Eval<Ir> buttonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createButton(Properties(properties.unlock)),
  _ => _createButton(Properties.empty()),
};

/// Create Button widget from properties object
Eval<Ir> _createButton(Properties properties) {
  final label = properties.label ?? 'Button'; // Extract label from properties
  final buttonWidget = ElevatedButton(
    onPressed: properties.disabled ? null : properties.onTap,
    child: Text(label),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}
