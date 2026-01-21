import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Text alignment enum object - represents all TextAlign values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter TextAlign enum value
final textAlign = IrObject({
  'left': IrNativeValue(HostValue(TextAlign.left)),
  'right': IrNativeValue(HostValue(TextAlign.right)),
  'center': IrNativeValue(HostValue(TextAlign.center)),
  'justify': IrNativeValue(HostValue(TextAlign.justify)),
  'start': IrNativeValue(HostValue(TextAlign.start)),
  'end': IrNativeValue(HostValue(TextAlign.end)),
});
