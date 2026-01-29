import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CupertinoActionSheet widget function
/// Creates Flutter CupertinoActionSheet from Glue expressions
/// Expects keyword arguments: :title, :message, :actions, etc.
final Ir cupertinoActionSheet = IrNativeFunc(cupertinoActionSheetImpl);

/// CupertinoActionSheet implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoActionSheetImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoActionSheet(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoActionSheet(Properties.empty()),
};

/// Create CupertinoActionSheet widget from properties object
Eval<Ir> _createCupertinoActionSheet(Properties properties) {
  return getRuntime().map((runtime) {
    final actionSheetWidget = CupertinoActionSheet(
      title: properties.cupertinoActionSheetTitle,
      message: properties.cupertinoActionSheetMessage,
      actions: properties.cupertinoActionSheetActions ?? [],
      messageScrollController:
          properties.cupertinoActionSheetMessageScrollController,
      actionScrollController:
          properties.cupertinoActionSheetActionScrollController,
      cancelButton: properties.cupertinoActionSheetCancelButton,
    );
    return IrNativeValue(Value(actionSheetWidget));
  });
}
