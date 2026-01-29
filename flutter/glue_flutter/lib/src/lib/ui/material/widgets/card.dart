import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// Card widget function
/// Creates Flutter Card from Glue (card props) expressions
final Ir card = IrNativeFunc(cardImpl);

/// Card implementation - takes properties object
Eval<Ir> cardImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCard(
    MaterialProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Card widget from properties
Eval<Ir> _createCard(MaterialProperties properties) {
  final cardWidget = Card(
    color: properties.color,
    shadowColor: properties.shadowColor,
    surfaceTintColor: properties.surfaceTintColor,
    elevation: properties.size, // using size for elevation
    shape: properties.shape,
    borderOnForeground: properties.borderOnForeground ?? true,
    margin: properties.margin,
    clipBehavior: properties.clipBehavior,
    semanticContainer: properties.semanticContainer ?? true,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(cardWidget)));
}
