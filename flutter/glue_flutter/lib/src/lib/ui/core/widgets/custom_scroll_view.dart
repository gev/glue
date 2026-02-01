import 'package:flutter/gestures.dart';
import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CustomScrollView widget function
/// Creates Flutter CustomScrollView from Glue (custom-scroll-view props) expressions
final Ir customScrollView = IrNativeFunc(customScrollViewImpl);

/// CustomScrollView implementation - takes properties object
Eval<Ir> customScrollViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCustomScrollView(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCustomScrollView(WidgetProperties.empty()),
};

/// Create CustomScrollView widget from properties
Eval<Ir> _createCustomScrollView(WidgetProperties properties) {
  final customScrollViewWidget = CustomScrollView(
    key: properties.key,
    scrollDirection:
        properties.getValue<Axis>('scroll-direction') ?? Axis.vertical,
    reverse: properties.getBool('reverse') ?? false,
    controller: properties.getValue<ScrollController>('controller'),
    primary: properties.getBool('primary'),
    physics: properties.getValue<ScrollPhysics>('physics'),
    shrinkWrap: properties.getBool('shrink-wrap') ?? false,
    center: properties.getKey('center'),
    anchor: properties.getDouble('anchor') ?? 0.0,
    cacheExtent: properties.getDouble('cache-extent'),
    slivers: properties.getWidgets('slivers'),
    semanticChildCount: properties.getInt('semantic-child-count'),
    dragStartBehavior:
        properties.getValue<DragStartBehavior>('drag-start-behavior') ??
        DragStartBehavior.start,
    keyboardDismissBehavior:
        properties.getValue<ScrollViewKeyboardDismissBehavior>(
          'keyboard-dismiss-behavior',
        ) ??
        ScrollViewKeyboardDismissBehavior.manual,
    restorationId: properties.getString('restoration-id'),
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
  );
  return Eval.pure(IrNativeValue(Value(customScrollViewWidget)));
}
