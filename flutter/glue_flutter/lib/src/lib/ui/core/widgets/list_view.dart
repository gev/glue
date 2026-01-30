import 'package:flutter/widgets.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ListView widget function
/// Creates Flutter ListView from Glue (list-view props) expressions
final Ir listView = IrNativeFunc(listViewImpl);

/// ListView implementation - takes properties object
Eval<Ir> listViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createListView(
    WidgetProperties(properties.unlock),
  ),
  _ => _createListView(WidgetProperties.empty()),
};

/// Create ListView widget from properties
Eval<Ir> _createListView(WidgetProperties properties) {
  final listViewWidget = ListView(
    key: properties.key,
    scrollDirection: properties.getValue('scroll-direction'),
    reverse: properties.getBool('reverse') ?? false,
    controller: properties.getValue('controller'),
    primary: properties.getBool('primary'),
    physics: properties.getValue('physics'),
    shrinkWrap: properties.getBool('shrink-wrap') ?? false,
    padding: properties.getValue('padding'),
    itemExtent: properties.getDouble('item-extent'),
    prototypeItem: properties.getWidget('prototype-item'),
    addAutomaticKeepAlives:
        properties.getBool('add-automatic-keep-alives') ?? true,
    addRepaintBoundaries: properties.getBool('add-repaint-boundaries') ?? true,
    addSemanticIndexes: properties.getBool('add-semantic-indexes') ?? true,
    cacheExtent: properties.getDouble('cache-extent'),
    children: properties.children,
    semanticChildCount: properties.getInt('semantic-child-count'),
    clipBehavior: properties.getValue('clip-behavior'),
  );
  return Eval.pure(IrNativeValue(Value(listViewWidget)));
}
