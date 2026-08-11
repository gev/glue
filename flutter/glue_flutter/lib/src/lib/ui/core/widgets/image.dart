import 'dart:io';

import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Image widget function
/// Creates Flutter Image from Glue (image props) expressions
final image = IrNativeFunc((Ir props) {
  switch (props) {
    case IrObject obj:
      final properties = WidgetProperties(obj.properties.unlock);
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
            properties.getValue<AlignmentGeometry>('alignment') ??
            Alignment.center,
        repeat:
            properties.getValue<ImageRepeat>('repeat') ?? ImageRepeat.noRepeat,
        centerSlice: properties.getValue<Rect>('center-slice'),
        matchTextDirection: properties.getBool('match-text-direction') ?? false,
        gaplessPlayback: properties.getBool('gapless-playback') ?? false,
        semanticLabel: properties.getString('semantic-label'),
        excludeFromSemantics:
            properties.getBool('exclude-from-semantics') ?? false,
        filterQuality:
            properties.getValue<FilterQuality>('filter-quality') ??
            FilterQuality.medium,
      );
      return Eval.pure(IrNativeValue(Value(imageWidget)));
    default:
      return throwError(wrongArgumentType(['object']));
  }
});

/// Image provider functions
final Ir imageAsset = IrNativeFunc(imageProviderImpl(assetImage));
final Ir imageFile = IrNativeFunc(imageProviderImpl(fileImage));
final Ir imageNetwork = IrNativeFunc(imageProviderImpl(networkImage));

Eval<Ir> Function(Ir props) imageProviderImpl(
  ImageProvider Function(String src) makeImage,
) => (Ir props) {
  switch (props) {
    case IrObject(:final properties):
      final src = properties['src'];
      switch (src) {
        case IrString(:final value):
        case IrSymbol(:final value):
        case IrDottedSymbol(:final value):
          return Eval.pure(IrNativeValue(Value(makeImage(value))));
        default:
          return throwError(
            wrongArgumentType([
              '`src` should be a `String`, `Symbol` or `DottedSymol`',
            ]),
          );
      }
    default:
      return throwError(wrongArgumentType(['object']));
  }
};

ImageProvider assetImage(String src) => AssetImage(src);
ImageProvider fileImage(String src) => FileImage(File(src));
ImageProvider networkImage(String src) => NetworkImage(src);
