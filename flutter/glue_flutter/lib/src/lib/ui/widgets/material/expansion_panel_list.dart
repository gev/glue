import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties_core.dart';

/// ExpansionPanelList widget function
/// Creates Flutter ExpansionPanelList from Glue (expansion-panel-list props) expressions
final Ir expansionPanelList = IrNativeFunc(expansionPanelListImpl);

/// ExpansionPanelList implementation - takes properties object
Eval<Ir> expansionPanelListImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createExpansionPanelList(
    Properties(properties.unlock),
  ),
  _ => _createExpansionPanelList(Properties.empty()),
};

/// Create ExpansionPanelList widget from properties
Eval<Ir> _createExpansionPanelList(Properties properties) {
  final expansionPanelListWidget = ExpansionPanelList(
    children: properties.expansionPanelListChildren ?? [],
    expansionCallback: properties.expansionPanelListExpansionCallback,
    animationDuration: properties.expansionPanelListAnimationDuration,
    elevation: properties.expansionPanelListElevation,
    materialGapSize: properties.expansionPanelListMaterialGapSize,
    dividerColor: properties.expansionPanelListDividerColor,
    expandIconColor: properties.expansionPanelListExpandIconColor,
  );
  return Eval.pure(IrNativeValue(Value(expansionPanelListWidget)));
}
