import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// TextStyle function - (text-style (:font-size 16 :color (rgb 255 0 0)))
final textStyle = IrNativeFunc(textStyleImpl);

Eval<Ir> textStyleImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createTextStyle(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createTextStyle(WidgetProperties properties) {
  final textStyle = TextStyle(
    inherit: properties.getBool('inherit') ?? true,
    color: properties.getColor('color'),
    backgroundColor: properties.getColor('background-color'),
    fontSize: properties.getDouble('font-size'),
    fontWeight: properties.getValue<FontWeight>('font-weight'),
    fontStyle: properties.getValue<FontStyle>('font-style'),
    letterSpacing: properties.getDouble('letter-spacing'),
    wordSpacing: properties.getDouble('word-spacing'),
    textBaseline: properties.getValue<TextBaseline>('text-baseline'),
    height: properties.getDouble('height'),
    leadingDistribution: properties.getValue<TextLeadingDistribution>(
      'leading-distribution',
    ),
    locale: properties.getValue<Locale>('locale'),
    foreground: properties.getValue<Paint>('foreground'),
    background: properties.getValue<Paint>('background'),
    shadows: properties.getValues<Shadow>('shadows'),
    fontFeatures: properties.getValues<FontFeature>('font-features'),
    fontVariations: properties.getValues<FontVariation>('font-variations'),
    decoration: properties.getValue<TextDecoration>('decoration'),
    decorationColor: properties.getColor('decoration-color'),
    decorationStyle: properties.getValue<TextDecorationStyle>(
      'decoration-style',
    ),
    decorationThickness: properties.getDouble('decoration-thickness'),
    debugLabel: properties.getString('debug-label'),
    fontFamily: properties.getString('font-family'),
    fontFamilyFallback: properties.getValues<String>('font-family-fallback'),
    package: properties.getString('package'),
  );
  return Eval.pure(IrNativeValue(Value(textStyle)));
}
