import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

/// TextInputAction enum object
/// Represents all TextInputAction values as Glue object properties
final textInputAction = IrObject({
  'none': IrNativeValue(Value(TextInputAction.none)),
  'unspecified': IrNativeValue(Value(TextInputAction.unspecified)),
  'done': IrNativeValue(Value(TextInputAction.done)),
  'search': IrNativeValue(Value(TextInputAction.search)),
  'send': IrNativeValue(Value(TextInputAction.send)),
  'next': IrNativeValue(Value(TextInputAction.next)),
  'previous': IrNativeValue(Value(TextInputAction.previous)),
  'continue-action': IrNativeValue(Value(TextInputAction.continueAction)),
  'join': IrNativeValue(Value(TextInputAction.join)),
  'route': IrNativeValue(Value(TextInputAction.route)),
  'emergency-call': IrNativeValue(Value(TextInputAction.emergencyCall)),
  'newline': IrNativeValue(Value(TextInputAction.newline)),
});
