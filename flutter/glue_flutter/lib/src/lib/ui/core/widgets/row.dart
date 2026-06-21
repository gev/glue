import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRow(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Row widget from properties
Eval<Ir> _createRow(WidgetProperties properties) {
  final rowWidget = Row(
    key: properties.key,
    children: properties.children,
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
    spacing: properties.getDouble('spacing') ?? 0,
    textBaseline: properties.getValue<TextBaseline>('text-baseline'),
  );
  return Eval.pure(IrNativeValue(Value(rowWidget)));
}
