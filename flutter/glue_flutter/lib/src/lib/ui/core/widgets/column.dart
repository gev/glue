import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createColumn(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Column widget from properties
Eval<Ir> _createColumn(WidgetProperties properties) {
  final columnWidget = Column(
    key: properties.key,
    mainAxisAlignment: properties.getValue('main-axis-alignment'),
    mainAxisSize: properties.getValue('main-axis-size'),
    crossAxisAlignment: properties.getValue('cross-axis-alignment'),
    textDirection: properties.getValue('text-direction'),
    verticalDirection: properties.getValue('vertical-direction'),
    textBaseline: properties.getValue('text-baseline'),
    children: properties.children,
  );
  return Eval.pure(IrNativeValue(Value(columnWidget)));
}
