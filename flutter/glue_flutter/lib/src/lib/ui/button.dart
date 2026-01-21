import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - requires positional label, optional properties
Eval<Ir> buttonImpl(Ir label) => switch (label) {
  IrString(:final value) => IrNativeFunc(
    (Ir props) => switch (props) {
      IrObject(:final properties) => _createButton(
        value,
        Properties(properties.unlock),
      ),
      _ => _createButton(value, Properties(null)),
    }(),
  ),
  _ => throwError(wrongArgumentType(['string'])),
};

/// Create Button widget from label and properties
Eval<Ir> _createButton(String label, Properties properties) {
  final buttonWidget = ElevatedButton(
    onPressed: properties.disabled ? null : properties.onTap,
    child: Text(label),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}
