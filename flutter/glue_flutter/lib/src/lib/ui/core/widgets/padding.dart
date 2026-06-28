import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Padding widget function
final Ir padding = IrNativeFunc(paddingImpl);

/// Padding implementation - takes properties object directly (just like card)
Eval<Ir> paddingImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPadding(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Padding widget from properties
Eval<Ir> _createPadding(WidgetProperties properties) {
  final paddingValue = properties.getValue<EdgeInsetsGeometry>('padding');
  if (paddingValue == null) {
    return throwError(wrongArgumentType(['padding property required']));
  }

  final paddingWidget = Padding(
    key: properties.key,
    padding: paddingValue,
    // WidgetProperties сам достанет :child из переданного объекта
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(paddingWidget)));
}
