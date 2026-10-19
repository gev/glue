import 'dart:ui';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ImageFilter.blur function
/// Creates an ImageFilter blur instance from Glue expressions
final Ir imageFilterBlur = IrNativeFunc((Ir props) {
  final properties = WidgetProperties(props);
  final imageFilter = ImageFilter.blur(
    sigmaX: properties.getDouble('sigma-x') ?? 0.0,
    sigmaY: properties.getDouble('sigma-y') ?? 0.0,
    tileMode: properties.getValue<TileMode>('tile-mode') ?? TileMode.clamp,
  );
  return Eval.pure(IrNativeValue(Value(imageFilter)));
});
