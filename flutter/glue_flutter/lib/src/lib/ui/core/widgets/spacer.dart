import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Spacer widget function
/// Creates Flutter Spacer from Glue (spacer props) expressions
final Ir spacer = IrNativeFunc(spacerImpl);

/// Spacer implementation - takes properties object
Eval<Ir> spacerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSpacer(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Spacer widget from properties
Eval<Ir> _createSpacer(WidgetProperties properties) {
  final spacerWidget = Spacer(
    key: properties.key,
    flex: properties.getInt('flex') ?? 1,
  );
  return Eval.pure(IrNativeValue(Value(spacerWidget)));
}
