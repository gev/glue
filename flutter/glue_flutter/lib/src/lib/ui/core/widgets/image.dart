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
  final image = properties.getValue('image');
  if (image == null) {
    throwError(wrongArgumentType(['image property required']));
  }

  final imageWidget = Image(
    key: properties.key,
    image: image,
    width: properties.width,
    height: properties.height,
    fit: properties.getValue('fit'),
    color: properties.getColor('color'),
    colorBlendMode: properties.getValue('color-blend-mode'),
    alignment: properties.getValue('alignment') ?? Alignment.center,
    repeat: properties.getValue('repeat') ?? ImageRepeat.noRepeat,
    centerSlice: properties.getValue('center-slice'),
    matchTextDirection: properties.getBool('match-text-direction') ?? false,
    gaplessPlayback: properties.getBool('gapless-playback') ?? false,
    semanticLabel: properties.getString('semantic-label'),
    excludeFromSemantics: properties.getBool('exclude-from-semantics') ?? false,
    filterQuality: properties.getValue('filter-quality') ?? FilterQuality.low,
  );
  return Eval.pure(IrNativeValue(Value(imageWidget)));
}
