import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// Text widget function
/// Creates Flutter Text widget from Glue (text :content "text" ...) expressions
final Ir text = IrNativeFunc(textImpl);

/// Text implementation - takes properties object with :content, :color, etc.
Eval<Ir> textImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createText(CoreProperties(properties.unlock)),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Text widget from properties (extracts :content, :color, etc.)
Eval<Ir> _createText(CoreProperties properties) {
  final content = properties.content ?? '';
  final textWidget = Text(
    content,
    style: TextStyle(
      color: properties.color,
      fontSize: properties.size,
      fontWeight: properties.weight,
    ),
    strutStyle: properties.strutStyle,
    textAlign: properties.align,
    textDirection: properties.textDirection,
    locale: properties.locale,
    softWrap: properties.softWrap,
    overflow: properties.overflow,
    textScaleFactor: properties.textScaleFactor,
    maxLines: properties.maxLines,
    semanticsLabel: properties.semanticsLabel,
    textWidthBasis: properties.textWidthBasis,
    textHeightBehavior: properties.textHeightBehavior,
  );
  return Eval.pure(IrNativeValue(Value(textWidget)));
}
