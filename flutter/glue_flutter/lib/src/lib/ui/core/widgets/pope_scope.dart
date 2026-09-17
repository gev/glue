import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// PopScope widget function
/// Creates Flutter PopScope from Glue (pope-scope props) expressions
final Ir popScope = IrNativeFunc(popScopeImpl);

/// PopScope implementation - takes properties object
Eval<Ir> popScopeImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPopScope(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create PopScope widget from properties
Eval<Ir> _createPopScope(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['Property `child` required']));
  }
  final popScopeWidget = PopScope(
    key: properties.key,
    canPop: properties.getBool('can-pop') ?? true,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(popScopeWidget)));
}
