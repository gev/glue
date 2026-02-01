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
    key: properties.key,
    scrollDirection:
        properties.getValue<Axis>('scroll-direction') ?? Axis.vertical,
    reverse: properties.getBool('reverse') ?? false,
    controller: properties.getValue<ScrollController>('controller'),
    primary: properties.getBool('primary'),
    physics: properties.getValue<ScrollPhysics>('physics'),
    shrinkWrap: properties.getBool('shrink-wrap') ?? false,
    padding: properties.getValue<EdgeInsetsGeometry>('padding'),
    gridDelegate:
        properties.getValue<SliverGridDelegate>('grid-delegate') ??
        const SliverGridDelegateWithFixedCrossAxisCount(crossAxisCount: 2),
    addAutomaticKeepAlives:
        properties.getBool('add-automatic-keep-alives') ?? true,
    addRepaintBoundaries: properties.getBool('add-repaint-boundaries') ?? true,
    addSemanticIndexes: properties.getBool('add-semantic-indexes') ?? true,
    cacheExtent: properties.getDouble('cache-extent'),
    children: properties.children,
    semanticChildCount: properties.getInt('semantic-child-count'),
    clipBehavior: properties.getValue<Clip>('clip-behavior') ?? Clip.none,
  );
  return Eval.pure(IrNativeValue(Value(gridViewWidget)));
}
