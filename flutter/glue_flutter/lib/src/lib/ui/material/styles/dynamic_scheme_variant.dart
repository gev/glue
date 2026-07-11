import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

final dynamicSchemeVariant = IrObject({
  'tonal-spot': IrNativeValue(Value(DynamicSchemeVariant.tonalSpot)),
  'horizontal': IrNativeValue(Value(DynamicSchemeVariant.fidelity)),
  'fidelity': IrNativeValue(Value(DynamicSchemeVariant.monochrome)),
  'neutral': IrNativeValue(Value(DynamicSchemeVariant.neutral)),
  'vibrant': IrNativeValue(Value(DynamicSchemeVariant.vibrant)),
  'expressive': IrNativeValue(Value(DynamicSchemeVariant.expressive)),
  'content': IrNativeValue(Value(DynamicSchemeVariant.content)),
  'rainbow': IrNativeValue(Value(DynamicSchemeVariant.rainbow)),
  'fruit-salad': IrNativeValue(Value(DynamicSchemeVariant.fruitSalad)),
});
