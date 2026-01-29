import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoPageScaffold widget function
/// Creates Flutter CupertinoPageScaffold from Glue expressions
/// Expects keyword arguments: :navigation-bar, :child, :background-color, etc.
final Ir cupertinoPageScaffold = IrNativeFunc(cupertinoPageScaffoldImpl);

/// CupertinoPageScaffold implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoPageScaffoldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoPageScaffold(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoPageScaffold(CupertinoProperties.empty()),
};

/// Create CupertinoPageScaffold widget from properties object
Eval<Ir> _createCupertinoPageScaffold(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final widget = CupertinoPageScaffold(
      navigationBar: properties.cupertinoPageScaffoldNavigationBar,
      backgroundColor: properties.cupertinoPageScaffoldBackgroundColor,
      resizeToAvoidBottomInset:
          properties.cupertinoPageScaffoldResizeToAvoidBottomInset,
      child: properties.cupertinoPageScaffoldChild ?? const SizedBox(),
    );
    return IrNativeValue(Value(widget));
  });
}
