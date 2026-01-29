import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/core_properties.dart';

/// CustomScrollView widget function
/// Creates Flutter CustomScrollView from Glue (custom-scroll-view props) expressions
final Ir customScrollView = IrNativeFunc(customScrollViewImpl);

/// CustomScrollView implementation - takes properties object
Eval<Ir> customScrollViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCustomScrollView(
    CoreProperties(properties.unlock),
  ),
  _ => _createCustomScrollView(CoreProperties.empty()),
};

/// Create CustomScrollView widget from properties
Eval<Ir> _createCustomScrollView(CoreProperties properties) {
  final customScrollViewWidget = CustomScrollView(
    scrollDirection: properties.customScrollViewScrollDirection,
    reverse: properties.customScrollViewReverse,
    controller: properties.customScrollViewController,
    primary: properties.customScrollViewPrimary,
    physics: properties.customScrollViewPhysics,
    shrinkWrap: properties.customScrollViewShrinkWrap,
    center: properties.customScrollViewCenter,
    anchor: properties.customScrollViewAnchor,
    cacheExtent: properties.customScrollViewCacheExtent,
    slivers: properties.customScrollViewSlivers ?? [],
    semanticChildCount: properties.customScrollViewSemanticChildCount,
    dragStartBehavior: properties.customScrollViewDragStartBehavior,
    keyboardDismissBehavior: properties.customScrollViewKeyboardDismissBehavior,
    restorationId: properties.customScrollViewRestorationId,
    clipBehavior: properties.customScrollViewClipBehavior,
  );
  return Eval.pure(IrNativeValue(Value(customScrollViewWidget)));
}
