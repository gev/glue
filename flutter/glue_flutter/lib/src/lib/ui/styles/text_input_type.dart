import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

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
  'visiblePassword': IrNativeValue(Value(TextInputType.visiblePassword)),
  'name': IrNativeValue(Value(TextInputType.name)),
  'streetAddress': IrNativeValue(Value(TextInputType.streetAddress)),
  'none': IrNativeValue(Value(TextInputType.none)),
});
