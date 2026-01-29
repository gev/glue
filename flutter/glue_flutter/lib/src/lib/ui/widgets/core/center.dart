import 'package:flutter/widgets.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Center widget function
/// Creates Flutter Center from Glue (center props) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes properties object
Eval<Ir> centerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCenter(
    CoreProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Center widget from properties
Eval<Ir> _createCenter(CoreProperties properties) {
  final centerWidget = Center(child: properties.child);
  return Eval.pure(IrNativeValue(Value(centerWidget)));
}
