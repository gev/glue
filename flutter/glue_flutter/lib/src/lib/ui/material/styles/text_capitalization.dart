import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

/// TextCapitalization enum object
/// Represents all TextCapitalization values as Glue object properties
final textCapitalization = IrObject({
  'none': IrNativeValue(Value(TextCapitalization.none)),
  'characters': IrNativeValue(Value(TextCapitalization.characters)),
  'words': IrNativeValue(Value(TextCapitalization.words)),
  'sentences': IrNativeValue(Value(TextCapitalization.sentences)),
});
