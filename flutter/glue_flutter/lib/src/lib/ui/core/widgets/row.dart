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
    children: properties.children,
    mainAxisAlignment: properties.mainAlign,
    mainAxisSize: properties.mainAxisSize,
    crossAxisAlignment: properties.crossAlign,
    textDirection: properties.textDirection,
    verticalDirection: properties.verticalDirection,
    textBaseline: properties.textBaseline,
  );
  return Eval.pure(IrNativeValue(Value(rowWidget)));
}
