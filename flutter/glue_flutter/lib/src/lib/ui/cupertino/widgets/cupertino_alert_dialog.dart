import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoAlertDialog widget function
/// Creates Flutter CupertinoAlertDialog from Glue expressions
/// Expects keyword arguments: :title, :content, :actions, etc.
final Ir cupertinoAlertDialog = IrNativeFunc(cupertinoAlertDialogImpl);

/// CupertinoAlertDialog implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoAlertDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoAlertDialog(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoAlertDialog(WidgetProperties.empty()),
};

/// Create CupertinoAlertDialog widget from properties object
Eval<Ir> _createCupertinoAlertDialog(WidgetProperties properties) {
  final alertDialogWidget = CupertinoAlertDialog(
    title: properties.getWidget('alert-dialog-title'),
    content: properties.getWidget('alert-dialog-content'),
    actions: properties.getWidgets('alert-dialog-actions') ?? [],
    scrollController: properties.getValue(
      'cupertino-alert-dialog-scroll-controller',
    ),
    actionScrollController: properties.getValue(
      'cupertino-alert-dialog-action-scroll-controller',
    ),
    insetAnimationDuration: properties.getValue(
      'cupertino-alert-dialog-inset-animation-duration',
    ),
    insetAnimationCurve: properties.getValue(
      'cupertino-alert-dialog-inset-animation-curve',
    ),
  );
  return Eval.pure(IrNativeValue(Value(alertDialogWidget)));
}
