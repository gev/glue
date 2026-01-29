import 'package:flutter/gestures.dart';
import 'package:glue/src/ir.dart';

/// Drag start behavior enum object - represents all DragStartBehavior values as Glue object properties
/// Each property is a NativeValue wrapping the Flutter DragStartBehavior enum value
final dragStartBehavior = IrObject({
  'start': IrNativeValue(Value(DragStartBehavior.start)),
  'down': IrNativeValue(Value(DragStartBehavior.down)),
});
