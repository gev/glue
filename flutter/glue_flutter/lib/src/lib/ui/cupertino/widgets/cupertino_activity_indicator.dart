import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/cupertino_properties.dart';

/// CupertinoActivityIndicator widget function
/// Creates Flutter CupertinoActivityIndicator from Glue expressions
/// Expects keyword arguments: :animating, :radius
final Ir cupertinoActivityIndicator = IrNativeFunc(
  cupertinoActivityIndicatorImpl,
);

/// CupertinoActivityIndicator implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoActivityIndicatorImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoActivityIndicator(
    CupertinoProperties(properties.unlock),
  ),
  _ => _createCupertinoActivityIndicator(CupertinoProperties.empty()),
};

/// Create CupertinoActivityIndicator widget from properties object
Eval<Ir> _createCupertinoActivityIndicator(CupertinoProperties properties) {
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
