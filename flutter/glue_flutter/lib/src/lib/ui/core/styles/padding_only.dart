import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Padding only function - (padding-only (:left 10 :top 5 :right 10 :bottom 5))
final paddingOnly = IrNativeFunc(paddingOnlyImpl);

Eval<Ir> paddingOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingOnly(
    CoreProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingOnly(CoreProperties properties) {
  final top = properties.top ?? 0;
  final right = properties.right ?? 0;
  final bottom = properties.bottom ?? 0;
  final left = properties.left ?? 0;
  return createPadding(
    EdgeInsets.only(top: top, left: left, bottom: bottom, right: right),
  );
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}
