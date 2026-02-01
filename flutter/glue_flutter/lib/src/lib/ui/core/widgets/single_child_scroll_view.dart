import 'package:flutter/gestures.dart';
import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// SingleChildScrollView widget function
/// Creates Flutter SingleChildScrollView from Glue (single-child-scroll-view props) expressions
final Ir singleChildScrollView = IrNativeFunc(singleChildScrollViewImpl);

/// SingleChildScrollView implementation - takes properties object
Eval<Ir> singleChildScrollViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSingleChildScrollView(
    WidgetProperties(properties.unlock),
  ),
  _ => _createSingleChildScrollView(WidgetProperties.empty()),
};

/// Create SingleChildScrollView widget from properties
Eval<Ir> _createSingleChildScrollView(WidgetProperties properties) {
  final singleChildScrollViewWidget = SingleChildScrollView(
    key: properties.key,
    scrollDirection:
        properties.getValue<Axis>('scroll-direction') ?? Axis.vertical,
    reverse: properties.getBool('reverse') ?? false,
    padding: properties.getValue<EdgeInsetsGeometry>('padding'),
    primary: properties.getBool('primary'),
    physics: properties.getValue<ScrollPhysics>('physics'),
    controller: properties.getValue<ScrollController>('controller'),
    dragStartBehavior:
        properties.getValue<DragStartBehavior>('drag-start-behavior') ??
        DragStartBehavior.start,
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
    restorationId: properties.getString('restoration-id'),
    keyboardDismissBehavior:
        properties.getValue<ScrollViewKeyboardDismissBehavior>(
          'keyboard-dismiss-behavior',
        ) ??
        ScrollViewKeyboardDismissBehavior.manual,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(singleChildScrollViewWidget)));
}
