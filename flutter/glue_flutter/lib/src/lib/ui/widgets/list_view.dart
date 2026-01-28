import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ListView widget function
/// Creates Flutter ListView from Glue (list-view props) expressions
final Ir listView = IrNativeFunc(listViewImpl);

/// ListView implementation - takes properties object
Eval<Ir> listViewImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createListView(Properties(properties.unlock)),
  _ => _createListView(Properties.empty()),
};

/// Create ListView widget from properties
Eval<Ir> _createListView(Properties properties) {
  final listViewWidget = ListView(
    scrollDirection: properties.listViewScrollDirection,
    reverse: properties.listViewReverse,
    controller: properties.listViewController,
    primary: properties.listViewPrimary,
    physics: properties.listViewPhysics,
    shrinkWrap: properties.listViewShrinkWrap,
    padding: properties.listViewPadding,
    itemExtent: properties.listViewItemExtent,
    prototypeItem: properties.listViewPrototypeItem,
    addAutomaticKeepAlives: properties.listViewAddAutomaticKeepAlives,
    addRepaintBoundaries: properties.listViewAddRepaintBoundaries,
    addSemanticIndexes: properties.listViewAddSemanticIndexes,
    cacheExtent: properties.listViewCacheExtent,
    children: properties.listViewChildren ?? [],
    semanticChildCount: properties.listViewSemanticChildCount,
    clipBehavior: properties.listViewClipBehavior,
  );
  return Eval.pure(IrNativeValue(Value(listViewWidget)));
}
