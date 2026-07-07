import 'package:flutter/material.dart';
import 'package:glue/context.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final Ir showModalBottomSheet_ = IrNativeFunc(showModalBottomSheetImpl);

Eval<Ir> showModalBottomSheetImpl(Ir ir) {
  return getRuntime().bind((runtime) {
    switch (ir) {
      case IrObject(:final properties):
        final context = getFromContext<BuildContext>(runtime.context);
        if (context != null) {
          return _showModalBottomSheet(
            context,
            WidgetProperties(properties.unlock),
          );
        }
        return Eval.pure(IrVoid());
      default:
        return throwError(wrongArgumentType(['Object parameter required']));
    }
  });
}

Eval<Ir> _showModalBottomSheet(
  BuildContext context,
  WidgetProperties properties,
) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`Widget` requared']));
  }
  showModalBottomSheet(
    context: context,
    builder: (_) => child,
    backgroundColor: properties.getValue<Color>('background-color'),
    elevation: properties.getDouble('elevation'),
    shape: properties.getValue<ShapeBorder>('shape'),
    clipBehavior: properties.getValue<Clip>('clip-behavior'),
    constraints: properties.getValue<BoxConstraints>('constraints'),
    barrierColor: properties.getValue<Color>('barrier-color'),
    barrierLabel: properties.getString('barrier-label'),
    isDismissible: properties.getBool('is-dismissible') ?? true,
    isScrollControlled: properties.getBool('is-scroll-controlled') ?? false,
    useSafeArea: properties.getBool('usesafe-area') ?? true,
    useRootNavigator: properties.getBool('use-root-navigator') ?? true,
    routeSettings: properties.getValue<RouteSettings>('route-settings'),
    anchorPoint: properties.getValue<Offset>('anchor-point'),
    requestFocus: properties.getBool('request-focus'),
    sheetAnimationStyle: properties.getValue<AnimationStyle>(
      'sheet-animation-style',
    ),
  );
  return Eval.pure(IrVoid());
}
