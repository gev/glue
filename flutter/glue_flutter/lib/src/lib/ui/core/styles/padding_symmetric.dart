import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Padding symmetric function - (padding-symmetric (:vertical 10 :horizontal 5))
final paddingSymmetric = IrNativeFunc(paddingSymmetricImpl);

Eval<Ir> paddingSymmetricImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingSymmetric(
    CoreProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingSymmetric(CoreProperties properties) {
  final vertical = properties.vertical ?? 0;
  final horizontal = properties.horizontal ?? 0;
  return createPadding(
    EdgeInsets.symmetric(vertical: vertical, horizontal: horizontal),
  );
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}
