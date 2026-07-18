import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Align widget function
/// Creates Flutter Align from Glue (align props) expressions
final Ir align = IrNativeFunc(alignImpl);

/// Align implementation - takes properties object
Eval<Ir> alignImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createAlign(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Align widget from properties
Eval<Ir> _createAlign(WidgetProperties properties) {
  final alignWidget = Align(
    key: properties.key,
    alignment:
        properties.getValue<AlignmentGeometry>('alignment') ?? Alignment.center,
    widthFactor: properties.getDouble('witdh-factor'),
    heightFactor: properties.getDouble('height-factor'),
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(alignWidget)));
}
