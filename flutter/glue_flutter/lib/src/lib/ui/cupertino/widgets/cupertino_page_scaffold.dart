import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoPageScaffold widget function
/// Creates Flutter CupertinoPageScaffold from Glue expressions
/// Expects keyword arguments: :navigation-bar, :child, :background-color, etc.
final Ir cupertinoPageScaffold = IrNativeFunc(cupertinoPageScaffoldImpl);

/// CupertinoPageScaffold implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoPageScaffoldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoPageScaffold(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoPageScaffold(WidgetProperties.empty()),
};

/// Create CupertinoPageScaffold widget from properties object
Eval<Ir> _createCupertinoPageScaffold(WidgetProperties properties) {
  final widget = CupertinoPageScaffold(
    key: properties.key,
    navigationBar: properties.getValue('navigation-bar'),
    backgroundColor: properties.getColor('background-color'),
    resizeToAvoidBottomInset:
        properties.getBool('resize-to-avoid-bottom-inset') ?? true,
    child: properties.child ?? const SizedBox(),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
