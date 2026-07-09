import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Hero widget function
/// Creates Flutter Hero from Glue (hero props) expressions
final Ir hero = IrNativeFunc(heroImpl);

/// Hero implementation - takes properties object
Eval<Ir> heroImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createHero(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create Hero widget from properties
Eval<Ir> _createHero(WidgetProperties properties) {
  final tag = properties.get('tag');
  print(tag);
  if (tag == null) {
    return throwError(wrongArgumentType(['`Object` tag property requiered']));
  }
  final child = properties.child;
  if (child == null) {
    return throwError(wrongArgumentType(['`Widget` child property required']));
  }
  final heroWidget = Hero(
    key: properties.key,
    tag: tag,
    createRectTween: properties.getValue<Tween<Rect?> Function(Rect?, Rect?)>(
      'create-rect-tween',
    ),
    flightShuttleBuilder: properties
        .getValue<
          Widget Function(
            BuildContext,
            Animation<double>,
            HeroFlightDirection,
            BuildContext,
            BuildContext,
          )
        >('flight-shuttle-builder'),
    placeholderBuilder: properties
        .getValue<Widget Function(BuildContext, Size, Widget)>(
          'placeholder-builder',
        ),
    transitionOnUserGestures:
        properties.getBool('transition-on-user-gestures') ?? false,
    curve: properties.getValue<Curve>('curve') ?? Curves.fastOutSlowIn,
    reverseCurve: properties.getValue<Curve>('reverse-cureve'),
    child: child,
  );
  return Eval.pure(IrNativeValue(Value(heroWidget)));
}
