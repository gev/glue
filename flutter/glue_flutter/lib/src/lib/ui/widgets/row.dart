import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRow(Properties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Row widget from properties
Eval<Ir> _createRow(Properties properties) {
  final rowWidget = Row(
    children: properties.children,
    mainAxisAlignment: properties.mainAlign,
    crossAxisAlignment: properties.crossAlign,
  );
  return Eval.pure(IrNativeValue(Value(rowWidget)));
}
