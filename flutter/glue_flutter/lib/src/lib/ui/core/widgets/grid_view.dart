import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// GridView widget function
/// Creates Flutter GridView from Glue (grid-view props) expressions
final Ir gridView = IrNativeFunc(gridViewImpl);

/// GridView implementation - takes properties object
Eval<Ir> gridViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createGridView(
    WidgetProperties(properties.unlock),
  ),
  _ => _createGridView(WidgetProperties.empty()),
};

/// Create GridView widget from properties
Eval<Ir> _createGridView(WidgetProperties properties) {
  final gridViewWidget = GridView(
    scrollDirection: properties.gridViewScrollDirection,
    reverse: properties.gridViewReverse,
    controller: properties.gridViewController,
    primary: properties.gridViewPrimary,
    physics: properties.gridViewPhysics,
    shrinkWrap: properties.gridViewShrinkWrap,
    padding: properties.gridViewPadding,
    gridDelegate:
        properties.gridViewGridDelegate ??
        const SliverGridDelegateWithFixedCrossAxisCount(crossAxisCount: 2),
    addAutomaticKeepAlives: properties.gridViewAddAutomaticKeepAlives,
    addRepaintBoundaries: properties.gridViewAddRepaintBoundaries,
    addSemanticIndexes: properties.gridViewAddSemanticIndexes,
    cacheExtent: properties.gridViewCacheExtent,
    children: properties.gridViewChildren ?? [],
    semanticChildCount: properties.gridViewSemanticChildCount,
    clipBehavior: properties.gridViewClipBehavior,
  );
  return Eval.pure(IrNativeValue(Value(gridViewWidget)));
}
