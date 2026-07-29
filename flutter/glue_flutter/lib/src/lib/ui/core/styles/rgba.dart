import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/color.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// RGBA function - (rgba 255 0 0 128)
final rgba = IrNativeFunc(rgbaImpl);

Eval<Ir> rgbaImpl(Ir r) => Eval.pure(
  IrNativeFunc(
    (Ir g) => Eval.pure(
      IrNativeFunc(
        (Ir b) =>
            Eval.pure(IrNativeFunc((Ir a) => createRgbaColor(r, g, b, a))),
      ),
    ),
  ),
);

Eval<Ir> createRgbaColor(Ir r, Ir g, Ir b, Ir a) {
  final alpha = extractInt(a)?.clamp(0, 255) ?? 255;
  final red = extractDouble(r)?.clamp(0, 1) ?? 0;
  final green = extractDouble(g)?.clamp(0, 1) ?? 0;
  final blue = extractDouble(b)?.clamp(0, 1) ?? 0;
  final color = Color.fromARGB(
    (255 * alpha).round(),
    (255 * red).round(),
    (255 * green).round(),
    (255 * blue).round(),
  );
  return Eval.pure(makeColor(color));
}
