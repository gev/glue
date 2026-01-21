import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Center widget function
/// Creates Flutter Center from Glue (center props) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes properties object
Eval<Ir> centerImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCenter(Properties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Center widget from properties
Eval<Ir> _createCenter(Properties properties) {
  final centerWidget = Center(child: properties.child);
  return Eval.pure(IrNativeValue(HostValue(centerWidget)));
}
