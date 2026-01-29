import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Padding directional function - (padding-directional (:start 10 :top 5 :end 10 :bottom 5))
final paddingDirectional = IrNativeFunc(paddingDirectionalImpl);

Eval<Ir> paddingDirectionalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingDirectional(
    CoreProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingDirectional(CoreProperties properties) {
  final top = properties.top ?? 0;
  final start = properties.start ?? 0;
  final bottom = properties.bottom ?? 0;
  final end = properties.end ?? 0;
  return createPadding(
    EdgeInsetsDirectional.only(
      start: start,
      top: top,
      end: end,
      bottom: bottom,
    ),
  );
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}
