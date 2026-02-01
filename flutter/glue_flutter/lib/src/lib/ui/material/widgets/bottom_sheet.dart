import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final onClosing = properties.getVoidCallback('on-closing');
  if (onClosing == null) {
    return throwError(wrongArgumentType(['on-closing']));
  }
  final WidgetBuilder? builder = properties.getValue<WidgetBuilder>('builder');
  if (builder == null) {
    return throwError(wrongArgumentType(['builder']));
  }
  return getRuntime().map((runtime) {
    final bottomSheetWidget = BottomSheet(
      key: properties.key,
      animationController: properties.getValue<AnimationController>(
        'animation-controller',
      ),
      enableDrag: properties.getBool('enable-drag') ?? false,
      showDragHandle: properties.getBool('show-drag-handle') ?? false,
      dragHandleColor: properties.getColor('drag-handle-color'),
      dragHandleSize: properties.getValue<Size>('drag-handle-size'),
      onDragStart: properties
          .getCallback<DragStartDetails>('on-drag-start')
          ?.call(runtime),
      backgroundColor: properties.getColor('background-color'),
      shadowColor: properties.getColor('shadow-color'),
      elevation: properties.getDouble('elevation'),
      shape: properties.getValue<ShapeBorder>('shape'),
      clipBehavior: properties.getValue<Clip>('clip-behavior'),
      constraints: properties.getValue<BoxConstraints>('constraints'),
      builder: builder,
      onClosing: onClosing(runtime),
    );
    return IrNativeValue(Value(bottomSheetWidget));
  });
}
