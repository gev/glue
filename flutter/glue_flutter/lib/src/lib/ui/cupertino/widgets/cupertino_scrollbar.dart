import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoScrollbar widget function
/// Creates Flutter CupertinoScrollbar from Glue (scrollbar props) expressions
/// Expects keyword arguments: :child, :controller, :thumb-visibility, etc.
final Ir cupertinoScrollbar = IrNativeFunc(cupertinoScrollbarImpl);

/// CupertinoScrollbar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoScrollbarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoScrollbar(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoScrollbar(CupertinoProperties.empty()),
};

/// Create CupertinoScrollbar widget from properties object
Eval<Ir> _createCupertinoScrollbar(CupertinoProperties properties) {
  return getRuntime().map((runtime) {
    final scrollbarWidget = CupertinoScrollbar(
      controller: properties.scrollController,
      thumbVisibility: properties.cupertinoScrollbarThumbVisibility,
      thickness: properties.cupertinoScrollbarThickness,
      thicknessWhileDragging:
          properties.cupertinoScrollbarThicknessWhileDragging,
      radius: properties.cupertinoScrollbarRadius,
      radiusWhileDragging: properties.cupertinoScrollbarRadiusWhileDragging,
      notificationPredicate: properties.scrollNotificationPredicate,
      child: properties.child ?? const Text('No child provided'),
    );
    return IrNativeValue(Value(scrollbarWidget));
  });
}
