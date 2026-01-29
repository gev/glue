import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Image widget function
/// Creates Flutter Image from Glue (image props) expressions
final Ir image = IrNativeFunc(imageImpl);

/// Image implementation - takes properties object
Eval<Ir> imageImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createImage(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Image widget from properties
Eval<Ir> _createImage(WidgetProperties properties) {
  final imageProvider = properties.getValue('image-provider');
  if (imageProvider == null) {
    throwError(wrongArgumentType(['image property required']));
  }

  final imageWidget = Image(
    key: properties.key,
    image: imageProvider!,
    width: properties.width,
    height: properties.height,
    fit: properties.getValue('box-fit'),
    color: properties.getColor('color'),
    colorBlendMode: properties.getValue('blend-mode'),
    alignment: properties.getValue('alignment') ?? Alignment.center,
    repeat: properties.getValue('image-repeat') ?? ImageRepeat.noRepeat,
    matchTextDirection: properties.getValue('match-text-direction') ?? false,
    gaplessPlayback: properties.getValue('gapless-playback') ?? false,
    semanticLabel: properties.getString('semantic-label'),
    excludeFromSemantics:
        properties.getValue('exclude-from-semantics') ?? false,
    filterQuality: properties.getValue('filter-quality') ?? FilterQuality.low,
  );
  return Eval.pure(IrNativeValue(Value(imageWidget)));
}
