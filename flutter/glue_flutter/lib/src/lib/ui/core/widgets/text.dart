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

/// Create Text widget from properties (extracts :content, :color, etc.)
Eval<Ir> _createText(WidgetProperties properties) {
  final content = properties.getString('content') ?? '';
  final textWidget = Text(
    content,
    key: properties.key,
    style: TextStyle(
      color: properties.getColor('color'),
      fontSize: properties.getDouble('size'),
      fontWeight: properties.getValue('weight'),
    ),
    strutStyle: properties.getValue('strut-style'),
    textAlign: properties.getValue('align'),
    textDirection: properties.getValue('text-direction'),
    locale: properties.getValue('locale'),
    softWrap: properties.getValue('soft-wrap'),
    overflow: properties.getValue('overflow'),
    textScaleFactor: properties.getValue('text-scale-factor'),
    maxLines: properties.getInt('max-lines'),
    semanticsLabel: properties.getString('semantics-label'),
    textWidthBasis: properties.getValue('text-width-basis'),
    textHeightBehavior: properties.getValue('text-height-behavior'),
  );
  return Eval.pure(IrNativeValue(Value(textWidget)));
}
