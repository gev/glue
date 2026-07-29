import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/ui/core/styles/color.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// RGB function - (rgb 255 0 0)
final rgb = IrNativeFunc(rgbImpl);

Eval<Ir> rgbImpl(Ir r) => Eval.pure(
  IrNativeFunc(
    (Ir g) => Eval.pure(IrNativeFunc((Ir b) => createRgbColor(r, g, b))),
  ),
);

Eval<Ir> createRgbColor(Ir r, Ir g, Ir b) {
  final red = extractDouble(r)?.clamp(0, 1) ?? 0;
  final green = extractDouble(g)?.clamp(0, 1) ?? 0;
  final blue = extractDouble(b)?.clamp(0, 1) ?? 0;
  final color = Color.fromARGB(
    255,
    (255 * red).round(),
    (255 * green).round(),
    (255 * blue).round(),
  );
  return Eval.pure(makeColor(color));
}
