import 'package:flutter/material.dart';
import 'package:glue/context.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final Ir showBottomSheet_ = IrNativeFunc(showBottomSheetImpl);

Eval<Ir> showBottomSheetImpl(Ir props) {
  return getRuntime().bind((runtime) {
    final context = getFromContext<BuildContext>(runtime.context);
    if (context != null) {
      return _showBottomSheet(context, WidgetProperties(props));
    }
    return Eval.pure(IrVoid());
  });
}

Eval<Ir> _showBottomSheet(BuildContext context, WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`Widget` requared']));
  }
  showBottomSheet(
    context: context,
    builder: (_) => child,
    backgroundColor: properties.getColor('background-color'),
    elevation: properties.getDouble('elevation'),
    shape: properties.getValue<ShapeBorder>('shape'),
    clipBehavior: properties.getValue<Clip>('clip-behavior'),
    constraints: properties.getValue<BoxConstraints>('constraints'),
    enableDrag: properties.getBool('enable-drag'),
    showDragHandle: properties.getBool('show-drag-habdle'),
    sheetAnimationStyle: properties.getValue<AnimationStyle>(
      'sheet-animation-style',
    ),
  );
  return Eval.pure(IrVoid());
}
