import 'package:flutter/cupertino.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSlidingSegmentedControl widget function
/// Creates Flutter CupertinoSlidingSegmentedControl from Glue expressions
/// Expects keyword arguments: :children, :on-value-changed, :group-value
final Ir cupertinoSlidingSegmentedControl = IrNativeFunc(
  cupertinoSlidingSegmentedControlImpl,
);

/// CupertinoSlidingSegmentedControl implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSlidingSegmentedControlImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSlidingSegmentedControl(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoSlidingSegmentedControl(WidgetProperties.empty()),
};

/// Create CupertinoSlidingSegmentedControl widget from properties object
Eval<Ir> _createCupertinoSlidingSegmentedControl(WidgetProperties properties) {
  final children = properties.getValue<Map<Object, Widget>>('children');
  if (children == null) {
    return throwError(wrongArgumentType(['Children required']));
  }
  final onValueChanged = properties.getCallback<dynamic>('on-value-changed');
  if (onValueChanged == null) {
    return throwError(wrongArgumentType(['on-value-changed required']));
  }
  return getRuntime().map((runtime) {
    final widget = CupertinoSlidingSegmentedControl(
      key: properties.key,
      children: children,
      onValueChanged: onValueChanged(runtime),
      groupValue: properties.getValue<dynamic>('group-value'),
      thumbColor: properties.getColor('thumb-color')!,
      backgroundColor:
          properties.getColor('background-color') ??
          CupertinoColors.tertiarySystemFill,
    );
    return IrNativeValue(Value(widget));
  });
}
