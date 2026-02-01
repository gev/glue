import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ExpansionPanelList widget function
/// Creates Flutter ExpansionPanelList from Glue (expansion-panel-list props) expressions
final Ir expansionPanelList = IrNativeFunc(expansionPanelListImpl);

/// ExpansionPanelList implementation - takes properties object
Eval<Ir> expansionPanelListImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createExpansionPanelList(
    WidgetProperties(properties.unlock),
  ),
  _ => _createExpansionPanelList(WidgetProperties.empty()),
};

/// Create ExpansionPanelList widget from properties
Eval<Ir> _createExpansionPanelList(WidgetProperties properties) {
  final expansionPanelListWidget = ExpansionPanelList(
    key: properties.key,

    children: (properties.getValue<>('children') as List<ExpansionPanel>?) ?? [],
    expansionCallback: properties.getValue<>('expansion-callback'),
    animationDuration: properties.getValue<>('animation-duration'),
    elevation: properties.getDouble('elevation') ?? 2.0,
    materialGapSize: properties.getDouble('material-gap-size') ?? 16.0,
    dividerColor: properties.getColor('divider-color'),
    expandIconColor: properties.getColor('expand-icon-color'),
  );
  return Eval.pure(IrNativeValue(Value(expansionPanelListWidget)));
}
