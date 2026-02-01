import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// BottomSheet widget function
/// Creates Flutter BottomSheet from Glue (bottom-sheet props) expressions
final Ir bottomSheet = IrNativeFunc(bottomSheetImpl);

/// BottomSheet implementation - takes properties object
Eval<Ir> bottomSheetImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBottomSheet(
    WidgetProperties(properties.unlock),
  ),
  _ => _createBottomSheet(WidgetProperties.empty()),
};

/// Create BottomSheet widget from properties
Eval<Ir> _createBottomSheet(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final bottomSheetWidget = BottomSheet(
      key: properties.key,
      animationController: properties.getValue<>('animation-controller'),
      enableDrag: properties.getBool('enable-drag') ?? false,
      showDragHandle: properties.getBool('show-drag-handle') ?? false,
      dragHandleColor: properties.getColor('drag-handle-color'),
      dragHandleSize: properties.getValue<>('drag-handle-size'),
      onDragStart: properties.getValue<>('on-drag-start'),
      onDragEnd: properties.getValue<>('on-drag-end'),
      backgroundColor: properties.getColor('background-color'),
      shadowColor: properties.getColor('shadow-color'),
      elevation: properties.getDouble('elevation'),
      shape: properties.getValue<>('shape'),
      clipBehavior: properties.getValue<>('clip-behavior'),
      constraints: properties.getValue<>('constraints'),
      onClosing: properties.getVoidCallback('on-closing', runtime)!,
      builder: properties.getValue<>('builder'),
    );
    return IrNativeValue(Value(bottomSheetWidget));
  });
}
