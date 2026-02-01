import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoActionSheet widget function
/// Creates Flutter CupertinoActionSheet from Glue expressions
/// Expects keyword arguments: :title, :message, :actions, etc.
final Ir cupertinoActionSheet = IrNativeFunc(cupertinoActionSheetImpl);

/// CupertinoActionSheet implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoActionSheetImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoActionSheet(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoActionSheet(WidgetProperties.empty()),
};

/// Create CupertinoActionSheet widget from properties object
Eval<Ir> _createCupertinoActionSheet(WidgetProperties properties) {
  final actionSheetWidget = CupertinoActionSheet(
    key: properties.key,
    title: properties.getWidget('title'),
    message: properties.getWidget('message'),
    actions: properties.getWidgets('actions'),
    messageScrollController: properties.getValue<ScrollController>(
      'message-scroll-controller',
    ),
    actionScrollController: properties.getValue<ScrollController>(
      'action-scroll-controller',
    ),
    cancelButton: properties.getWidget('cancel-button'),
  );
  return Eval.pure(IrNativeValue(Value(actionSheetWidget)));
}
