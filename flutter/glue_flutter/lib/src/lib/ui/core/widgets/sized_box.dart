import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SizedBox widget function
/// Creates Flutter SizedBox from Glue (sized-box props) expressions
final Ir sizedBox = IrNativeFunc(sizedBoxImpl);

/// SizedBox implementation - takes properties object
Eval<Ir> sizedBoxImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSizedBox(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create SizedBox widget from properties
Eval<Ir> _createSizedBox(WidgetProperties properties) {
  final sizedBoxWidget = SizedBox(
    key: properties.key,
    width: properties.width,
    height: properties.height,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(sizedBoxWidget)));
}
