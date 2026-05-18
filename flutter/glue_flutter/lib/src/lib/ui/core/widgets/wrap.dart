import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Wrap widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir wrap = IrNativeFunc(rowImpl);

/// Wrap implementation - takes properties object
Eval<Ir> rowImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createWrap(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Wrap widget from properties
Eval<Ir> _createWrap(WidgetProperties properties) {
  final rowWidget = Wrap(
    key: properties.key,
    children: properties.children,
    direction: properties.getValue<Axis>('direction') ?? Axis.horizontal,
    alignment:
        properties.getValue<WrapAlignment>('alignment') ?? WrapAlignment.start,
    runAlignment:
        properties.getValue<WrapAlignment>('run-alignment') ??
        WrapAlignment.start,
    textDirection: properties.getValue<TextDirection>('text-direction'),
    verticalDirection:
        properties.getValue<VerticalDirection>('vertical-direction') ??
        VerticalDirection.down,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
  );
  return Eval.pure(IrNativeValue(Value(rowWidget)));
}
