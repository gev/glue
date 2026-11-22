import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';

/// TextInputType enum object
/// Represents all TextInputType values as Glue object properties
final textInputType = IrObject({
  'text': IrNativeValue(Value(TextInputType.text)),
  'multiline': IrNativeValue(Value(TextInputType.multiline)),
  'number': IrNativeValue(Value(TextInputType.number)),
  'phone': IrNativeValue(Value(TextInputType.phone)),
  'datetime': IrNativeValue(Value(TextInputType.datetime)),
  'email': IrNativeValue(Value(TextInputType.emailAddress)),
  'url': IrNativeValue(Value(TextInputType.url)),
  'visible-password': IrNativeValue(Value(TextInputType.visiblePassword)),
  'name': IrNativeValue(Value(TextInputType.name)),
  'street-address': IrNativeValue(Value(TextInputType.streetAddress)),
  'none': IrNativeValue(Value(TextInputType.none)),
});
