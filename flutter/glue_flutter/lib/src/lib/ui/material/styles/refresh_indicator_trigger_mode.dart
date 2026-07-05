import 'package:flutter/material.dart';
import 'package:glue/ir.dart';

/// RefreshIndicatorTriggerMode enum object
/// Represents all RefreshIndicatorTriggerMode values as Glue object properties
final refreshIndicatorTriggerMode = IrObject({
  'anywhere': IrNativeValue(Value(RefreshIndicatorTriggerMode.anywhere)),
  'on-edge': IrNativeValue(Value(RefreshIndicatorTriggerMode.onEdge)),
});
