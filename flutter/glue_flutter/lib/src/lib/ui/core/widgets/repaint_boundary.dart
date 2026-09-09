import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// RepaintBoundary widget function
/// Creates Flutter RepaintBoundary from Glue (sized-box props) expressions
final Ir repaintBoundary = IrNativeFunc(repaintBoundaryImpl);

/// RepaintBoundary implementation - takes properties object
Eval<Ir> repaintBoundaryImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createRepaintBoundary(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create RepaintBoundary widget from properties
Eval<Ir> _createRepaintBoundary(WidgetProperties properties) {
  final repaintBoundaryWidget = RepaintBoundary(
    key: properties.key,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(repaintBoundaryWidget)));
}
