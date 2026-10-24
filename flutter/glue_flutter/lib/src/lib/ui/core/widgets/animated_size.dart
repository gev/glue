import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// AnimatedPositioned widget function
final Ir animatedPositioned = IrNativeFunc(animatedPositionedImpl);

/// AnimatedPositioned implementation - takes properties object directly (just like card)
Eval<Ir> animatedPositionedImpl(Ir props) =>
    _createAnimatedPositioned(WidgetProperties(props));

/// Create Positioned widget from properties
Eval<Ir> _createAnimatedPositioned(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`child` property required']));
  }
  final duration = properties.getValue<Duration>('duration');
  if (duration == null) {
    return throwError(wrongArgumentType(['`duration` property required']));
  }

  final positionedWidget = AnimatedSize(
    key: properties.key,
    duration: duration,
    alignment:
        properties.getValue<AlignmentGeometry>('alignment') ??
        AlignmentGeometry.center,
    curve: properties.getValue<Curve>('curve') ?? Curves.linear,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(positionedWidget)));
}
