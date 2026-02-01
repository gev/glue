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
  final content = properties.child;
  if (content == null) {
    throwError(wrongArgumentType(['content property required']));
  }

  return getRuntime().map((runtime) {
    final snackBarWidget = SnackBar(
      key: properties.key,
      content: content!,
      backgroundColor: properties.getColor('background-color'),
      elevation: properties.getDouble('elevation'),
      margin: properties.getValue<EdgeInsetsGeometry>('margin'),
      padding: properties.getValue<EdgeInsetsGeometry>('padding'),
      width: properties.width,
      shape: properties.getValue<ShapeBorder>('shape'),
      behavior: properties.getValue<SnackBarBehavior>('behavior'),
      action: properties.getValue<SnackBarAction>('action'),
      animation: properties.getValue<AnimationController>('animation'),
      onVisible: properties.getVoidCallback('on-visible')?.call(runtime),
      dismissDirection:
          properties.getValue<DismissDirection>('dismiss-direction') ??
          DismissDirection.down,
      clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
    );
    return IrNativeValue(Value(snackBarWidget));
  });
}
