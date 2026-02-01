import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoSegmentedControl widget function
/// Creates Flutter CupertinoSegmentedControl from Glue expressions
/// Expects keyword arguments: :children, :on-value-changed, :group-value
final Ir cupertinoSegmentedControl = IrNativeFunc(
  cupertinoSegmentedControlImpl,
);

/// CupertinoSegmentedControl implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoSegmentedControlImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoSegmentedControl(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoSegmentedControl(WidgetProperties.empty()),
};

/// Create CupertinoSegmentedControl widget from properties object
Eval<Ir> _createCupertinoSegmentedControl(WidgetProperties properties) {
  final widget = CupertinoSegmentedControl(
    key: properties.key,
    children: properties.getValue<>('children'),
    onValueChanged: properties.getValue<>('on-value-changed'),
    groupValue: properties.getValue<>('group-value'),
    unselectedColor: properties.getColor('unselected-color'),
    selectedColor: properties.getColor('selected-color'),
    borderColor: properties.getColor('border-color'),
    pressedColor: properties.getColor('pressed-color'),
    padding: properties.getValue<>('padding'),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
