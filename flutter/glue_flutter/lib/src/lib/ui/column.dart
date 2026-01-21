import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object or children list
Eval<Ir> columnImpl(Ir arg) => switch (arg) {
  IrObject(:final properties) => _createColumn(Properties(properties.unlock)),
  IrList(:final elements) => _createColumn(
    Properties({'children': IrList(elements.toList())}),
  ),
  _ => throwError(wrongArgumentType(['object', 'list'])),
};

/// Create Column widget from properties
Eval<Ir> _createColumn(Properties properties) {
  final columnWidget = Column(
    children: properties.children,
    mainAxisAlignment: properties.mainAlign,
    crossAxisAlignment: properties.crossAlign,
  );
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}
