import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createColumn(
    CoreProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Column widget from properties
Eval<Ir> _createColumn(CoreProperties properties) {
  final columnWidget = Column(
    children: properties.children,
    mainAxisAlignment: properties.mainAlign,
    mainAxisSize: properties.mainAxisSize,
    crossAxisAlignment: properties.crossAlign,
    textDirection: properties.textDirection,
    verticalDirection: properties.verticalDirection,
    textBaseline: properties.textBaseline,
  );
  return Eval.pure(IrNativeValue(Value(columnWidget)));
}
