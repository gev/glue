import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Padding widget function
/// Creates Flutter Padding from Glue (padding child props) expressions
final Ir padding = IrNativeFunc(paddingImpl);

/// Padding implementation - takes child, then properties
Eval<Ir> paddingImpl(Ir child) {
  return Eval.pure(IrNativeFunc(paddingWithChild(child)));
}

/// Padding with child - takes properties object
Eval<Ir> Function(Ir) paddingWithChild(Ir child) =>
    (Ir props) => switch (props) {
      IrObject(:final properties) => _createPadding(
        WidgetProperties(properties.unlock),
      ),
      _ => throwError(wrongArgumentType(['object'])),
    };

/// Create Padding widget from properties and child
Eval<Ir> _createPadding(WidgetProperties properties) {
  final paddingWidget = Padding(
    padding: properties.padding,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(paddingWidget)));
}
