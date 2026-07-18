import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Positioned widget function
final Ir positioned = IrNativeFunc(positionedImpl);

/// Positioned implementation - takes properties object directly (just like card)
Eval<Ir> positionedImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPositioned(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Positioned widget from properties
Eval<Ir> _createPositioned(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`child` property required']));
  }

  final positionedWidget = Positioned(
    key: properties.key,
    left: properties.left,
    right: properties.right,
    top: properties.top,
    bottom: properties.bottom,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(positionedWidget)));
}
