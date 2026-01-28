import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Image widget function
/// Creates Flutter Image from Glue (image props) expressions
final Ir image = IrNativeFunc(imageImpl);

/// Image implementation - takes properties object
Eval<Ir> imageImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createImage(Properties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Image widget from properties
Eval<Ir> _createImage(Properties properties) {
  final imageProvider = properties.imageProvider;
  if (imageProvider == null) {
    throwError(wrongArgumentType(['image property required']));
  }

  final imageWidget = Image(
    image: imageProvider!,
    width: properties.width,
    height: properties.height,
    fit: properties.boxFit,
    color: properties.color,
    colorBlendMode: properties.blendMode,
    alignment: properties.alignment ?? Alignment.center,
    repeat: properties.imageRepeat ?? ImageRepeat.noRepeat,
    matchTextDirection: properties.matchTextDirection ?? false,
    gaplessPlayback: properties.gaplessPlayback ?? false,
    semanticLabel: properties.semanticsLabel,
    excludeFromSemantics: properties.excludeFromSemantics ?? false,
    filterQuality: properties.filterQuality ?? FilterQuality.low,
  );
  return Eval.pure(IrNativeValue(Value(imageWidget)));
}
