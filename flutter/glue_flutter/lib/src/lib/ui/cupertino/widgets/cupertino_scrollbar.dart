import 'package:flutter/cupertino.dart';
import 'package:glue/error.dart';
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
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['Child required']));
  }
  final scrollbarWidget = CupertinoScrollbar(
    key: properties.key,
    controller: properties.getValue<ScrollController>('controller'),
    thumbVisibility: properties.getBool('thumb-visibility'),
    thickness:
        properties.getDouble('thickness') ??
        CupertinoScrollbar.defaultThickness,
    thicknessWhileDragging:
        properties.getDouble('thickness-while-dragging') ??
        CupertinoScrollbar.defaultThicknessWhileDragging,
    radius:
        properties.getValue<Radius>('radius') ??
        CupertinoScrollbar.defaultRadius,
    radiusWhileDragging:
        properties.getValue<Radius>('radius-while-dragging') ??
        CupertinoScrollbar.defaultRadiusWhileDragging,
    notificationPredicate: properties.getValue<bool Function(Notification)>(
      'notification-predicate',
    ),
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(scrollbarWidget)));
}
