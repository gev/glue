import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes label
Eval<Ir> buttonImpl(Ir label) {
  return Eval.pure(IrNativeFunc(buttonWithLabel(label)));
}

/// Button with label - takes optional properties
Eval<Ir> Function(Ir) buttonWithLabel(Ir label) =>
    (Ir props) => switch ((label, props)) {
      (IrString(:final value), IrObject(:final properties)) => _createButton(
        value,
        Properties(properties.unlock),
      ),
      (IrString(:final value), _) => _createButton(value, Properties(null)),
      _ => throwError(wrongArgumentType(['string', 'object?'])),
    };

/// Create Button widget from label and properties
Eval<Ir> _createButton(String label, Properties properties) {
  final buttonWidget = ElevatedButton(
    onPressed: properties.disabled ? null : properties.onTap,
    child: Text(label),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}
