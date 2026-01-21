import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object (optional)
Eval<Ir> buttonImpl(Ir arg) => switch (arg) {
  IrObject(:final properties) => _createButton(Properties(properties.unlock)),
  IrString(:final value) => _createButton(
    Properties({'label': IrString(value)}),
  ),
  _ => throwError(wrongArgumentType(['object', 'string'])),
};

/// Create Button widget from properties
Eval<Ir> _createButton(Properties properties) {
  final buttonWidget = ElevatedButton(
    onPressed: properties.disabled ? null : properties.onTap,
    child: Text(properties.label),
  );
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}
