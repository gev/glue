import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoScrollbar widget function
/// Creates Flutter CupertinoScrollbar from Glue (scrollbar props) expressions
/// Expects keyword arguments: :child, :controller, :thumb-visibility, etc.
final Ir cupertinoScrollbar = IrNativeFunc(cupertinoScrollbarImpl);

/// CupertinoScrollbar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoScrollbarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoScrollbar(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoScrollbar(Properties.empty()),
};

/// Create CupertinoScrollbar widget from properties object
Eval<Ir> _createCupertinoScrollbar(Properties properties) {
  return getRuntime().map((runtime) {
    final scrollbarWidget = CupertinoScrollbar(
      controller: properties.scrollController,
      thumbVisibility: properties.cupertinoScrollbarThumbVisibility,
      thickness: properties.cupertinoScrollbarThickness ?? 3.0,
      thicknessWhileDragging:
          properties.cupertinoScrollbarThicknessWhileDragging ?? 8.0,
      radius: properties.cupertinoScrollbarRadius ?? const Radius.circular(1.5),
      radiusWhileDragging:
          properties.cupertinoScrollbarRadiusWhileDragging ??
          const Radius.circular(4.0),
      notificationPredicate: properties.scrollNotificationPredicate,
      child: properties.child ?? const Text('No child provided'),
    );
    return IrNativeValue(Value(scrollbarWidget));
  });
}
