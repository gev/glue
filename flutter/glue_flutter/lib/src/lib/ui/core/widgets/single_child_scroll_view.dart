import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// SingleChildScrollView widget function
/// Creates Flutter SingleChildScrollView from Glue (single-child-scroll-view props) expressions
final Ir singleChildScrollView = IrNativeFunc(singleChildScrollViewImpl);

/// SingleChildScrollView implementation - takes properties object
Eval<Ir> singleChildScrollViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createSingleChildScrollView(
    CoreProperties(properties.unlock),
  ),
  _ => _createSingleChildScrollView(CoreProperties.empty()),
};

/// Create SingleChildScrollView widget from properties
Eval<Ir> _createSingleChildScrollView(CoreProperties properties) {
  final singleChildScrollViewWidget = SingleChildScrollView(
    scrollDirection: properties.singleChildScrollViewScrollDirection,
    reverse: properties.singleChildScrollViewReverse,
    padding: properties.singleChildScrollViewPadding,
    primary: properties.singleChildScrollViewPrimary,
    physics: properties.singleChildScrollViewPhysics,
    controller: properties.singleChildScrollViewController,
    dragStartBehavior: properties.singleChildScrollViewDragStartBehavior,
    clipBehavior: properties.singleChildScrollViewClipBehavior,
    restorationId: properties.singleChildScrollViewRestorationId,
    keyboardDismissBehavior:
        properties.singleChildScrollViewKeyboardDismissBehavior,
    child: properties.singleChildScrollViewChild,
  );
  return Eval.pure(IrNativeValue(Value(singleChildScrollViewWidget)));
}
