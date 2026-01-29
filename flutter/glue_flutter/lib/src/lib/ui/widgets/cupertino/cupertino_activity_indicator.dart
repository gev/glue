import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CupertinoActivityIndicator widget function
/// Creates Flutter CupertinoActivityIndicator from Glue expressions
/// Expects keyword arguments: :animating, :radius
final Ir cupertinoActivityIndicator = IrNativeFunc(
  cupertinoActivityIndicatorImpl,
);

/// CupertinoActivityIndicator implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoActivityIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoActivityIndicator(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoActivityIndicator(Properties.empty()),
};

/// Create CupertinoActivityIndicator widget from properties object
Eval<Ir> _createCupertinoActivityIndicator(Properties properties) {
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
