import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Blend function - (blend color1 color2 alphaBlend)
/// Blends two colors with alpha blending
final blend = IrNativeFunc(blendImpl);

Eval<Ir> blendImpl(Ir c1) => Eval.pure(
  IrNativeFunc(
    (Ir c2) =>
        Eval.pure(IrNativeFunc((Ir alpha) => blendColors(c1, c2, alpha))),
  ),
);

Eval<Ir> blendColors(Ir c1, Ir c2, Ir alpha) {
  final color1 = extractColor(c1);
  final color2 = extractColor(c2);
  final alphaValue = (extractDouble(alpha)?.clamp(0, 1) ?? 0.5).toDouble();

  if (color1 != null && color2 != null) {
    // Perform alpha blending
    final blendedColor = Color.alphaBlend(
      color1.withOpacity(alphaValue),
      color2,
    );
    return Eval.pure(IrNativeValue(Value(blendedColor)));
  }

  return Eval.pure(IrString('Error: Invalid colors for blending'));
}
