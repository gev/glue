import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Card widget function
/// Creates Flutter Card from Glue (card props) expressions
final Ir card = IrNativeFunc(cardImpl);

/// Card implementation - takes properties object
Eval<Ir> cardImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCard(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Card widget from properties
Eval<Ir> _createCard(WidgetProperties properties) {
  final cardWidget = Card(
    key: properties.key,
    color: properties.getColor('color'),
    shadowColor: properties.getColor('shadow-color'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    elevation: properties.getDouble('elevation'),
    shape: properties.getValue<>('shape'),
    borderOnForeground: properties.getBool('border-on-foreground') ?? true,
    margin: properties.getValue<>('margin'),
    clipBehavior: properties.getValue<>('clip-behavior'),
    semanticContainer: properties.getBool('semantic-container') ?? true,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(cardWidget)));
}
