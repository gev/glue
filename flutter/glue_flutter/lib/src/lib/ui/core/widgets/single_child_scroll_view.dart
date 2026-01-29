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
    scrollDirection: properties.getValue('scroll-direction'),
    reverse: properties.getValue('reverse'),
    padding: properties.getValue('padding'),
    primary: properties.getValue('primary'),
    physics: properties.getValue('physics'),
    controller: properties.getValue('controller'),
    dragStartBehavior: properties.getValue('drag-start-behavior'),
    clipBehavior: properties.getValue('clip-behavior'),
    restorationId: properties.getString('restoration-id'),
    keyboardDismissBehavior: properties.getValue('keyboard-dismiss-behavior'),
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(singleChildScrollViewWidget)));
}
