import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Padding directional function - (padding-directional (:start 10 :top 5 :end 10 :bottom 5))
final paddingDirectional = IrNativeFunc(paddingDirectionalImpl);

Eval<Ir> paddingDirectionalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingDirectional(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingDirectional(WidgetProperties properties) {
  final insets = EdgeInsetsDirectional.only(
    start: properties.start ?? 0,
    top: properties.top ?? 0,
    end: properties.end ?? 0,
    bottom: properties.bottom ?? 0,
  );
  return Eval.pure(IrNativeValue(Value(insets)));
}
