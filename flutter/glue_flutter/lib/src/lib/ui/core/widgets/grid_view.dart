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
    scrollDirection: properties.getValue('scroll-direction'),
    reverse: properties.getValue('reverse'),
    controller: properties.getValue('controller'),
    primary: properties.getValue('primary'),
    physics: properties.getValue('physics'),
    shrinkWrap: properties.getValue('shrink-wrap'),
    padding: properties.getValue('padding'),
    gridDelegate:
        properties.getValue('grid-delegate') ??
        const SliverGridDelegateWithFixedCrossAxisCount(crossAxisCount: 2),
    addAutomaticKeepAlives: properties.getValue('add-automatic-keep-alives'),
    addRepaintBoundaries: properties.getValue('add-repaint-boundaries'),
    addSemanticIndexes: properties.getValue('add-semantic-indexes'),
    cacheExtent: properties.getDouble('cache-extent'),
    children: properties.children,
    semanticChildCount: properties.getInt('semantic-child-count'),
    clipBehavior: properties.getValue('clip-behavior'),
  );
  return Eval.pure(IrNativeValue(Value(gridViewWidget)));
}
