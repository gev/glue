import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Icon widget function
/// Creates Flutter Icon from Glue (icon props) expressions
final Ir icon = IrNativeFunc(iconImpl);

/// Icon implementation - takes properties object
Eval<Ir> iconImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createIcon(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Icon widget from properties
Eval<Ir> _createIcon(WidgetProperties properties) {
  final iconData = properties.getValue('icon');
  if (iconData == null) {
    throwError(wrongArgumentType(['icon property required']));
  }

  final iconWidget = Icon(
    iconData,
    key: properties.key,
    size: properties.getDouble('size'),
    color: properties.getColor('color'),
    semanticLabel: properties.getString('semantic-label'),
    textDirection: properties.getValue('text-direction'),
  );
  return Eval.pure(IrNativeValue(Value(iconWidget)));
}
