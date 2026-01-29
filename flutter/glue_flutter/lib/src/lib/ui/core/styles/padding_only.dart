import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Padding only function - (padding-only (:left 10 :top 5 :right 10 :bottom 5))
final paddingOnly = IrNativeFunc(paddingOnlyImpl);

Eval<Ir> paddingOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingOnly(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingOnly(WidgetProperties properties) {
  final insets = EdgeInsets.only(
    top: properties.top ?? 0,
    left: properties.left ?? 0,
    bottom: properties.bottom ?? 0,
    right: properties.right ?? 0,
  );
  return Eval.pure(IrNativeValue(Value(insets)));
}
