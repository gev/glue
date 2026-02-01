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
    mainAxisAlignment:
        properties.getValue<MainAxisAlignment>('main-axis-alignment') ??
        MainAxisAlignment.start,
    mainAxisSize:
        properties.getValue<MainAxisSize>('main-axis-size') ?? MainAxisSize.max,
    crossAxisAlignment:
        properties.getValue<CrossAxisAlignment>('cross-axis-alignment') ??
        CrossAxisAlignment.center,
    textDirection: properties.getValue<TextDirection>('text-direction'),
    verticalDirection:
        properties.getValue<VerticalDirection>('vertical-direction') ??
        VerticalDirection.down,
    textBaseline: properties.getValue<TextBaseline>('text-baseline'),
    children: properties.children,
  );
  return Eval.pure(IrNativeValue(Value(columnWidget)));
}
