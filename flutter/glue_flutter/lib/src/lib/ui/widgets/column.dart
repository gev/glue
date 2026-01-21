import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createColumn(Properties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
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
