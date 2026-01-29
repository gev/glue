import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// RefreshIndicatorTriggerMode enum object
/// Represents all RefreshIndicatorTriggerMode values as Glue object properties
final refreshIndicatorTriggerMode = IrObject({
  'anywhere': IrNativeValue(Value(RefreshIndicatorTriggerMode.anywhere)),
  'onEdge': IrNativeValue(Value(RefreshIndicatorTriggerMode.onEdge)),
});
