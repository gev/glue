import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Opacity widget function
/// Creates Flutter Opacity from Glue (opacity props) expressions
final Ir opacity = IrNativeFunc(opacityImpl);

/// Opacity implementation - takes properties object
Eval<Ir> opacityImpl(Ir props) => _createOpacity(WidgetProperties(props));

/// Create Opacity widget from properties
Eval<Ir> _createOpacity(WidgetProperties properties) {
  final opacityWidget = Opacity(
    key: properties.key,
    opacity: properties.getDouble('opacity') ?? 1,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(opacityWidget)));
}
