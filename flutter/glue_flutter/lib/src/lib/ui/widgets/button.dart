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

  if (properties.disabled) {
    final buttonWidget = ElevatedButton(onPressed: null, child: Text(label));
    return Eval.pure(IrNativeValue(Value(buttonWidget)));
  }

  // Get runtime and create callback
  return getRuntime().map((runtime) {
    final callback = properties.onTap(runtime);
    final buttonWidget = ElevatedButton(
      onPressed: callback,
      child: Text(label),
    );
    return IrNativeValue(Value(buttonWidget));
  });
}
