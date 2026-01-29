import 'package:flutter/widgets.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// HSL function - (hsl 360 1.0 0.5)
final hsl = IrNativeFunc(hslImpl);

Eval<Ir> hslImpl(Ir h) => Eval.pure(
  IrNativeFunc(
    (Ir s) => Eval.pure(IrNativeFunc((Ir l) => createHslColor(h, s, l))),
  ),
);

Eval<Ir> createHslColor(Ir h, Ir s, Ir l) {
  final hue = (extractDouble(h)?.clamp(0, 360) ?? 0).toDouble();
  final saturation = (extractDouble(s)?.clamp(0, 1) ?? 0).toDouble();
  final lightness = (extractDouble(l)?.clamp(0, 1) ?? 0).toDouble();
  final color = HSLColor.fromAHSL(1.0, hue, saturation, lightness).toColor();
  return Eval.pure(IrNativeValue(Value(color)));
}
