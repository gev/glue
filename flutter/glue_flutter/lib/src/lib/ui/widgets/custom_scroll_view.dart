import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// CustomScrollView widget function
/// Creates Flutter CustomScrollView from Glue (custom-scroll-view props) expressions
final Ir customScrollView = IrNativeFunc(customScrollViewImpl);

/// CustomScrollView implementation - takes properties object
Eval<Ir> customScrollViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCustomScrollView(
    Properties(properties.unlock),
  ),
  _ => _createCustomScrollView(Properties.empty()),
};

/// Create CustomScrollView widget from properties
Eval<Ir> _createCustomScrollView(Properties properties) {
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
