import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Padding symmetric function - (padding-symmetric (:vertical 10 :horizontal 5))
final paddingSymmetric = IrNativeFunc(paddingSymmetricImpl);

Eval<Ir> paddingSymmetricImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingSymmetric(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingSymmetric(WidgetProperties properties) {
  final insets = EdgeInsets.symmetric(
    vertical: properties.vertical ?? 0,
    horizontal: properties.horizontal ?? 0,
  );
  return Eval.pure(IrNativeValue(Value(insets)));
}
