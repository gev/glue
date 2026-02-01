import 'package:flutter/cupertino.dart';
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
  final widget = CupertinoSlidingSegmentedControl(
    key: properties.key,
    children: properties.getValue<>('children'),
    onValueChanged: properties.getValue<>('on-value-changed'),
    groupValue: properties.getValue<>('group-value'),
    thumbColor: properties.getColor('thumb-color')!,
    backgroundColor:
        properties.getColor('background-color') ??
        CupertinoColors.tertiarySystemFill,
    padding: properties.getValue<>('padding'),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
