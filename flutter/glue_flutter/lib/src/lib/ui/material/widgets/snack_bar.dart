import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SnackBar widget function
/// Creates Flutter SnackBar from Glue (snack-bar props) expressions
final Ir snackBar = IrNativeFunc(snackBarImpl);

/// SnackBar implementation - takes properties object
Eval<Ir> snackBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSnackBar(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create SnackBar widget from properties
Eval<Ir> _createSnackBar(WidgetProperties properties) {
  final content = properties.getWidget('content');
  if (content == null) {
    throwError(wrongArgumentType(['content property required']));
  }

  return getRuntime().map((runtime) {
    final snackBarWidget = SnackBar(
      key: properties.key,
      content: content!,
      backgroundColor: properties.getColor('background-color'),
      elevation: properties.getDouble('elevation'),
      margin: properties.getValue('margin'),
      padding: properties.getValue('padding'),
      width: properties.getDouble('width'),
      shape: properties.getValue('shape'),
      behavior: properties.getValue('behavior'),
      action: properties.getValue('action'),
      duration: properties.getValue('duration') ?? const Duration(seconds: 4),
      animation: properties.getValue('animation'),
      onVisible: properties.getVoidCallback('on-visible', runtime),
      dismissDirection:
          properties.getValue('dismiss-direction') ?? DismissDirection.down,
      clipBehavior: properties.getValue('clip-behavior') ?? Clip.hardEdge,
    );
    return IrNativeValue(Value(snackBarWidget));
  });
}
