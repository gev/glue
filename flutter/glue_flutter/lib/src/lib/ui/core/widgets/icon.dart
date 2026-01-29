import 'package:flutter/widgets.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Icon widget function
/// Creates Flutter Icon from Glue (icon props) expressions
final Ir icon = IrNativeFunc(iconImpl);

/// Icon implementation - takes properties object
Eval<Ir> iconImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createIcon(CoreProperties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Icon widget from properties
Eval<Ir> _createIcon(CoreProperties properties) {
  final iconData = properties.icon;
  if (iconData == null) {
    throwError(wrongArgumentType(['icon property required']));
  }

  final iconWidget = Icon(
    iconData,
    size: properties.size,
    color: properties.color,
    semanticLabel: properties.semanticsLabel,
    textDirection: properties.textDirection,
  );
  return Eval.pure(IrNativeValue(Value(iconWidget)));
}
