import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Cross-axis alignment enum object - represents all CrossAxisAlignment values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter CrossAxisAlignment enum value
final crossAxisAlignment = IrObject({
  'start': IrNativeValue(HostValue(CrossAxisAlignment.start)),
  'end': IrNativeValue(HostValue(CrossAxisAlignment.end)),
  'center': IrNativeValue(HostValue(CrossAxisAlignment.center)),
  'stretch': IrNativeValue(HostValue(CrossAxisAlignment.stretch)),
  'baseline': IrNativeValue(HostValue(CrossAxisAlignment.baseline)),
});
