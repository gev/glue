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
  final image = properties.getValue<ImageProvider<Object>>('image');
  if (image == null) {
    return throwError(wrongArgumentType(['image property required']));
  }
  final imageWidget = Image(
    key: properties.key,
    image: image,
    width: properties.width,
    height: properties.height,
    fit: properties.getValue<BoxFit>('fit'),
    color: properties.getColor('color'),
    colorBlendMode: properties.getValue<BlendMode>('color-blend-mode'),
    alignment:
        properties.getValue<AlignmentGeometry>('alignment') ?? Alignment.center,
    repeat: properties.getValue<ImageRepeat>('repeat') ?? ImageRepeat.noRepeat,
    centerSlice: properties.getValue<Rect>('center-slice'),
    matchTextDirection: properties.getBool('match-text-direction') ?? false,
    gaplessPlayback: properties.getBool('gapless-playback') ?? false,
    semanticLabel: properties.getString('semantic-label'),
    excludeFromSemantics: properties.getBool('exclude-from-semantics') ?? false,
    filterQuality:
        properties.getValue<FilterQuality>('filter-quality') ??
        FilterQuality.medium,
  );
  return Eval.pure(IrNativeValue(Value(imageWidget)));
}
