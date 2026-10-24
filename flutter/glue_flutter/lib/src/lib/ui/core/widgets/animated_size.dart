import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// AnimatedSize widget function
final Ir animatedSize = IrNativeFunc(animatedSizeImpl);

/// AnimatedSize implementation - takes properties object directly (just like card)
Eval<Ir> animatedSizeImpl(Ir props) =>
    _createAnimatedSize(WidgetProperties(props));

/// Create AnimatedSize widget from properties
Eval<Ir> _createAnimatedSize(WidgetProperties properties) {
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`child` property required']));
  }
  final duration = properties.getValue<Duration>('duration');
  if (duration == null) {
    return throwError(wrongArgumentType(['`duration` property required']));
  }

  final widget = AnimatedSize(
    key: properties.key,
    duration: duration,
    alignment:
        properties.getValue<AlignmentGeometry>('alignment') ??
        AlignmentGeometry.center,
    curve: properties.getValue<Curve>('curve') ?? Curves.linear,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.hardEdge,
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
