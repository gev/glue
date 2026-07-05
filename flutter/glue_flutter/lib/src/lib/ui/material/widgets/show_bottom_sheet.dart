import 'package:flutter/material.dart';
import 'package:glue/context.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final Ir showBottomSheet_ = IrNativeFunc(showBottomSheetImpl);

Eval<Ir> showBottomSheetImpl(Ir ir) {
  return getRuntime().bind((runtime) {
    switch (ir) {
      case IrObject(:final properties):
        final context = getFromContext<BuildContext>(runtime.context);
        if (context != null) {
          return _showBottomSheet(context, WidgetProperties(properties.unlock));
        }
        return Eval.pure(IrVoid());
      default:
        return throwError(wrongArgumentType(['Object parameter required']));
    }
  });
}

Eval<Ir> _showBottomSheet(BuildContext context, WidgetProperties properties) {
  final bottomSheet = properties.getValue<Widget>('bottomSheet');
  if (bottomSheet == null) {
    return throwError(wrongArgumentType(['`Widget` requared']));
  }
  showBottomSheet(
    context: context,
    builder: (_) => bottomSheet,
    backgroundColor: properties.getValue<Color>('background-color'),
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
