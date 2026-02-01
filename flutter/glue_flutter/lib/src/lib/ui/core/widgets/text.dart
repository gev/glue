import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Text widget function
/// Creates Flutter Text widget from Glue (text :content "text" ...) expressions
final Ir text = IrNativeFunc(textImpl);

/// Text implementation - takes properties object with :content, :color, etc.
Eval<Ir> textImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createText(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Text widget from properties
Eval<Ir> _createText(WidgetProperties properties) {
  final textWidget = Text(
    properties.getString('data') ?? '',
    key: properties.key,
    style: properties.getValue<TextStyle>('style'),
    strutStyle: properties.getValue<StrutStyle>('strut-style'),
    textAlign: properties.getValue<TextAlign>('text-align'),
    textDirection: properties.getValue<TextDirection>('text-direction'),
    locale: properties.getValue<Locale>('locale'),
    softWrap: properties.getBool('soft-wrap'),
    overflow: properties.getValue<TextOverflow>('overflow'),
    maxLines: properties.getInt('max-lines'),
    semanticsLabel: properties.getString('semantics-label'),
    textWidthBasis: properties.getValue<TextWidthBasis>('text-width-basis'),
    textHeightBehavior: properties.getValue<TextHeightBehavior>(
      'text-height-behavior',
    ),
  );
  return Eval.pure(IrNativeValue(Value(textWidget)));
}
