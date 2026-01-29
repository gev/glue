import 'package:flutter/widgets.dart';
import 'package:glue/src/ir.dart';

/// Text baseline enum object - represents all TextBaseline values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TextBaseline enum value
final textBaseline = IrObject({
  'alphabetic': IrNativeValue(Value(TextBaseline.alphabetic)),
  'ideographic': IrNativeValue(Value(TextBaseline.ideographic)),
});
