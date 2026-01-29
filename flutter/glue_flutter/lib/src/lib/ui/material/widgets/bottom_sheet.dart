import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// BottomSheet widget function
/// Creates Flutter BottomSheet from Glue (bottom-sheet props) expressions
final Ir bottomSheet = IrNativeFunc(bottomSheetImpl);

/// BottomSheet implementation - takes properties object
Eval<Ir> bottomSheetImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createBottomSheet(
    MaterialProperties(properties.unlock),
  ),
  _ => _createBottomSheet(MaterialProperties.empty()),
};

/// Create BottomSheet widget from properties
Eval<Ir> _createBottomSheet(MaterialProperties properties) {
  final bottomSheetWidget = BottomSheet(
    animationController: properties.bottomSheetAnimationController,
    enableDrag: properties.bottomSheetEnableDrag,
    showDragHandle: properties.bottomSheetShowDragHandle,
    dragHandleColor: properties.bottomSheetDragHandleColor,
    dragHandleSize: properties.bottomSheetDragHandleSize,
    onDragStart: properties.bottomSheetOnDragStart,
    onDragEnd: properties.bottomSheetOnDragEnd,
    backgroundColor: properties.refreshBackgroundColor,
    shadowColor: properties.drawerShadowColor,
    elevation: properties.drawerElevation,
    shape: properties.drawerShape,
    clipBehavior: properties.drawerClipBehavior,
    constraints: properties.popupMenuConstraints,
    onClosing: properties.bottomSheetOnClosing,
    builder: properties.bottomSheetBuilder,
  );
  return Eval.pure(IrNativeValue(Value(bottomSheetWidget)));
}
