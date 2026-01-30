import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoScrollbar widget function
/// Creates Flutter CupertinoScrollbar from Glue (scrollbar props) expressions
/// Expects keyword arguments: :child, :controller, :thumb-visibility, etc.
final Ir cupertinoScrollbar = IrNativeFunc(cupertinoScrollbarImpl);

/// CupertinoScrollbar implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoScrollbarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoScrollbar(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoScrollbar(WidgetProperties.empty()),
};

/// Create CupertinoScrollbar widget from properties object
Eval<Ir> _createCupertinoScrollbar(WidgetProperties properties) {
  final scrollbarWidget = CupertinoScrollbar(
    key: properties.key,
    controller: properties.getValue('controller'),
    thumbVisibility: properties.getBool('thumb-visibility') ?? false,
    thickness: properties.getDouble('thickness') ?? 3.0,
    thicknessWhileDragging:
        properties.getDouble('thickness-while-dragging') ?? 8.0,
    radius: properties.getValue('radius'),
    radiusWhileDragging: properties.getValue('radius-while-dragging'),
    notificationPredicate: properties.getValue('notification-predicate'),
    child: properties.child ?? const Text('No child provided'),
  );
  return Eval.pure(IrNativeValue(Value(scrollbarWidget)));
}
