import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// SnackBar widget function
/// Creates Flutter SnackBar from Glue (snack-bar props) expressions
final Ir snackBar = IrNativeFunc(snackBarImpl);

/// SnackBar implementation - takes properties object
Eval<Ir> snackBarImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSnackBar(
    MaterialProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create SnackBar widget from properties
Eval<Ir> _createSnackBar(MaterialProperties properties) {
  if (properties.snackBarContent == null) {
    throwError(wrongArgumentType(['content property required']));
  }

  return getRuntime().map((runtime) {
    final snackBarWidget = SnackBar(
      content: properties.snackBarContent!,
      backgroundColor: properties.color,
      elevation: properties.size, // using size for elevation
      margin: properties.margin,
      padding: properties.padding,
      width: properties.width,
      shape: properties.shape,
      behavior: properties.snackBarBehavior,
      action: properties.snackBarAction,
      duration: properties.snackBarDuration ?? const Duration(seconds: 4),
      animation: properties.snackBarAnimation,
      onVisible: properties.onVisible(runtime),
      dismissDirection: properties.dismissDirection ?? DismissDirection.down,
      clipBehavior: properties.clipBehavior,
    );
    return IrNativeValue(Value(snackBarWidget));
  });
}
