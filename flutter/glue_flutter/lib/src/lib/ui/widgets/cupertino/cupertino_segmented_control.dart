import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
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
    Properties(properties.unlock),
  ),
  _ => _createCupertinoSegmentedControl(Properties.empty()),
};

/// Create CupertinoSegmentedControl widget from properties object
Eval<Ir> _createCupertinoSegmentedControl(Properties properties) {
  final widget = CupertinoSegmentedControl(
    children: properties.cupertinoSegmentedControlChildren,
    onValueChanged:
        properties.cupertinoSegmentedControlOnValueChanged ?? (_) {},
    groupValue: properties.cupertinoSegmentedControlGroupValue,
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
