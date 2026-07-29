import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/color.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// HSV function - (hsv 360 1.0 1.0)
final hsva = IrNativeFunc(hsvaImpl);

Eval<Ir> hsvaImpl(Ir h) => Eval.pure(
  IrNativeFunc(
    (Ir s) => Eval.pure(
      IrNativeFunc(
        (Ir l) =>
            Eval.pure(IrNativeFunc((Ir a) => createHsvaColor(h, s, l, a))),
      ),
    ),
  ),
);

Eval<Ir> createHsvaColor(Ir h, Ir s, Ir v, Ir a) {
  final hue = (extractDouble(h)?.clamp(0, 360) ?? 0).toDouble();
  final saturation = (extractDouble(s)?.clamp(0, 1) ?? 0).toDouble();
  final value = (extractDouble(v)?.clamp(0, 1) ?? 1).toDouble();
  final alpha = (extractDouble(a)?.clamp(0, 1) ?? 1).toDouble();
  final color = HSVColor.fromAHSV(alpha, hue, saturation, value).toColor();
  return Eval.pure(makeColor(color));
}
