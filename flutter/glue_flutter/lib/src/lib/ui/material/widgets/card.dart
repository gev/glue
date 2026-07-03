import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Card widget function
/// Creates Flutter Card from Glue (card props) expressions
final Ir card = IrNativeFunc(cardImpl(Card.new));
final Ir cardFilled = IrNativeFunc(cardImpl(Card.filled));
final Ir cardOutlined = IrNativeFunc(cardImpl(Card.outlined));

/// Card implementation - takes properties object
Eval<Ir> Function(Ir props) cardImpl(dynamic card) =>
    (Ir props) => switch (props) {
      IrObject(:final properties) => _createCard(
        card,
        WidgetProperties(properties.unlock),
      ),
      _ => throwError(wrongArgumentType(['object'])),
    };

/// Create Card widget from properties
Eval<Ir> _createCard(dynamic card, WidgetProperties properties) {
  final cardWidget = card(
    key: properties.key,
    color: properties.getColor('color'),
    shadowColor: properties.getColor('shadow-color'),
    surfaceTintColor: properties.getColor('surface-tint-color'),
    elevation: properties.getDouble('elevation'),
    shape: properties.getValue<ShapeBorder>('shape'),
    borderOnForeground: properties.getBool('border-on-foreground') ?? true,
    margin: properties.getValue<EdgeInsetsGeometry>('margin'),
    clipBehavior: properties.getValue<Clip>('clip-behavior'),
    semanticContainer: properties.getBool('semantic-container') ?? true,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(cardWidget)));
}
