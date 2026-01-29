import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// ExpansionPanelList widget function
/// Creates Flutter ExpansionPanelList from Glue (expansion-panel-list props) expressions
final Ir expansionPanelList = IrNativeFunc(expansionPanelListImpl);

/// ExpansionPanelList implementation - takes properties object
Eval<Ir> expansionPanelListImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createExpansionPanelList(
    MaterialProperties(properties.unlock),
  ),
  _ => _createExpansionPanelList(MaterialProperties.empty()),
};

/// Create ExpansionPanelList widget from properties
Eval<Ir> _createExpansionPanelList(MaterialProperties properties) {
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
