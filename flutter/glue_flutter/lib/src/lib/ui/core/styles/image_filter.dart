import 'dart:ui';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ImageFilter.blur function
/// Creates an ImageFilter blur instance from Glue expressions
final Ir imageFilterBlur = IrNativeFunc((Ir props) {
  switch (props) {
    case IrObject(:final properties):
      final props = WidgetProperties(properties.unlock);
      final imageFilter = ImageFilter.blur(
        sigmaX: props.getValue<double>('sigma-x') ?? 0.0,
        sigmaY: props.getValue<double>('sigma-y') ?? 0.0,
        tileMode: props.getValue<TileMode>('tile-mode') ?? TileMode.clamp,
      );
      return Eval.pure(IrNativeValue(Value(imageFilter)));
    default:
      return throwError(wrongArgumentType(['object']));
  }
});
