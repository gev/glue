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
    title: properties.getWidget('cupertino-action-sheet-title'),
    message: properties.getWidget('cupertino-action-sheet-message'),
    actions: properties.getWidgets('cupertino-action-sheet-actions') ?? [],
    messageScrollController: properties.getValue(
      'cupertino-action-sheet-message-scroll-controller',
    ),
    actionScrollController: properties.getValue(
      'cupertino-action-sheet-action-scroll-controller',
    ),
    cancelButton: properties.getWidget('cupertino-action-sheet-cancel-button'),
  );
  return Eval.pure(IrNativeValue(Value(actionSheetWidget)));
}
