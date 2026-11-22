import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter_ui/utils/image_provider.dart';
import 'package:glue_flutter_ui/utils/widget_properties.dart';

/// Image widget function
/// Creates Flutter Image from Glue (image props) expressions
final image = IrNativeFunc((Ir props) {
  final properties = WidgetProperties(props);
  final image = properties.getValue<ImageProvider>('image');
  if (image == null) {
    return throwError(
      wrongArgumentType(['Image provider property `image` required']),
    );
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
});

/// Image provider functions
final Ir imageAsset = IrNativeFunc(imageProviderImpl(assetImage));
final Ir imageFile = IrNativeFunc(imageProviderImpl(fileImage));
final Ir imageNetwork = IrNativeFunc(imageProviderImpl(networkImage));
