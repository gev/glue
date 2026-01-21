import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object or children list
Eval<Ir> rowImpl(Ir arg) => switch (arg) {
  IrObject(:final properties) => _createRow(Properties(properties.unlock)),
  IrList(:final elements) => _createRow(
    Properties({'children': IrList(elements.toList())}),
  ),
  _ => throwError(wrongArgumentType(['object', 'list'])),
};

/// Create Row widget from properties
Eval<Ir> _createRow(Properties properties) {
  final rowWidget = Row(
    children: properties.children,
    mainAxisAlignment: properties.mainAlign,
    crossAxisAlignment: properties.crossAlign,
  );
  return Eval.pure(IrNativeValue(HostValue(rowWidget)));
}
