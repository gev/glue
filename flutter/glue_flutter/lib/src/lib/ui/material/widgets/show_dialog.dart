import 'package:flutter/material.dart';
import 'package:glue/context.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';

final Ir showDialog_ = IrNativeFunc(showDialogImpl);

Eval<Ir> showDialogImpl(Ir ir) {
  return getRuntime().bind((runtime) {
    switch (ir) {
      case IrObject(:final properties):
        final context = getFromContext<BuildContext>(runtime.context);
        if (context != null) {
          return _showDialog(context, WidgetProperties(properties.unlock));
        }
        return Eval.pure(IrVoid());
      default:
        return throwError(wrongArgumentType(['Object parameter required']));
    }
  });
}

Eval<Ir> _showDialog(BuildContext context, WidgetProperties properties) {
  final dialog = properties.child;
  if (dialog == null) {
    return throwError(wrongArgumentType(['`Widget` requared']));
  }
  showDialog(
    context: context,
    builder: (_) => dialog,
    barrierDismissible: properties.getBool('barrier-dismissible') ?? true,
    barrierColor: properties.getValue<Color>('barrier-color'),
    barrierLabel: properties.getString('barrier-label'),
    useSafeArea: properties.getBool('usesafe-area') ?? true,
    useRootNavigator: properties.getBool('use-root-navigator') ?? true,
    routeSettings: properties.getValue<RouteSettings>('route-settings'),
    anchorPoint: properties.getValue<Offset>('anchor-point'),
    traversalEdgeBehavior: properties.getValue<TraversalEdgeBehavior>(
      'traversal-edge-behavior',
    ),
    fullscreenDialog: properties.getBool('fullscreen-dialog') ?? false,
    requestFocus: properties.getBool('request-focus'),
    animationStyle: properties.getValue<AnimationStyle>('animation-style'),
  );
  return Eval.pure(IrVoid());
}
