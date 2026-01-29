import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoActivityIndicator widget function
/// Creates Flutter CupertinoActivityIndicator from Glue expressions
/// Expects keyword arguments: :animating, :radius
final Ir cupertinoActivityIndicator = IrNativeFunc(
  cupertinoActivityIndicatorImpl,
);

/// CupertinoActivityIndicator implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoActivityIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoActivityIndicator(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoActivityIndicator(WidgetProperties.empty()),
};

/// Create CupertinoActivityIndicator widget from properties object
Eval<Ir> _createCupertinoActivityIndicator(WidgetProperties properties) {
  return Eval.pure(
    IrNativeValue(
      Value(
        CupertinoActivityIndicator(
          animating: properties.cupertinoActivityIndicatorAnimating,
          radius: properties.cupertinoActivityIndicatorRadius,
        ),
      ),
    ),
  );
}
