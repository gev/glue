import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SafeArea widget function
/// Creates Flutter SafeArea from Glue (safe-area props) expressions
final Ir safeArea = IrNativeFunc(safeAreaImpl);

/// SafeArea implementation - takes properties object
Eval<Ir> safeAreaImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSafeArea(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create SafeArea widget from properties
Eval<Ir> _createSafeArea(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['Property `child` required']));
  }
  final safeAreaWidget = SafeArea(
    key: properties.key,
    left: properties.getBool('left') ?? true,
    top: properties.getBool('top') ?? true,
    right: properties.getBool('right') ?? true,
    bottom: properties.getBool('bottom') ?? true,
    minimum: properties.getValue<EdgeInsets>('minimum') ?? EdgeInsets.zero,
    maintainBottomViewPadding:
        properties.getBool('maintainBottomViewPadding') ?? false,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(safeAreaWidget)));
}
