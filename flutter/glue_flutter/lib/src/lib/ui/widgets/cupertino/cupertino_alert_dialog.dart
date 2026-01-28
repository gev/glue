import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoAlertDialog widget function
/// Creates Flutter CupertinoAlertDialog from Glue expressions
/// Expects keyword arguments: :title, :content, :actions, etc.
final Ir cupertinoAlertDialog = IrNativeFunc(cupertinoAlertDialogImpl);

/// CupertinoAlertDialog implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoAlertDialogImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoAlertDialog(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoAlertDialog(Properties.empty()),
};

/// Create CupertinoAlertDialog widget from properties object
Eval<Ir> _createCupertinoAlertDialog(Properties properties) {
  return getRuntime().map((runtime) {
    final alertDialogWidget = CupertinoAlertDialog(
      title: properties.alertDialogTitle,
      content: properties.alertDialogContent,
      actions: properties.alertDialogActions ?? [],
      scrollController: properties.cupertinoAlertDialogScrollController,
      actionScrollController:
          properties.cupertinoAlertDialogActionScrollController,
      insetAnimationDuration:
          properties.cupertinoAlertDialogInsetAnimationDuration,
      insetAnimationCurve: properties.cupertinoAlertDialogInsetAnimationCurve,
    );
    return IrNativeValue(Value(alertDialogWidget));
  });
}
