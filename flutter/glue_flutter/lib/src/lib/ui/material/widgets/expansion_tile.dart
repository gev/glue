import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ExpansionTile widget function
/// Creates Flutter ExpansionTile from Glue (expansion-tile props) expressions
final Ir expansionTile = IrNativeFunc(expansionTileImpl);

/// ExpansionTile implementation - takes properties object
Eval<Ir> expansionTileImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createExpansionTile(
    WidgetProperties(properties.unlock),
  ),
  _ => _createExpansionTile(WidgetProperties.empty()),
};

/// Create ExpansionTile widget from properties
Eval<Ir> _createExpansionTile(WidgetProperties properties) {
  final expansionTileWidget = ExpansionTile(
    leading: properties.expansionTileLeading,
    title: properties.expansionTileTitle ?? const SizedBox(),
    subtitle: properties.expansionTileSubtitle,
    trailing: properties.expansionTileTrailing,
    children: properties.expansionTileChildren ?? [],
    initiallyExpanded: properties.expansionTileInitiallyExpanded,
    maintainState: properties.expansionTileMaintainState,
    tilePadding: properties.expansionTileTilePadding,
    expandedAlignment: properties.expansionTileExpandedAlignment,
    expandedCrossAxisAlignment:
        properties.expansionTileExpandedCrossAxisAlignment,
    childrenPadding: properties.expansionTileChildrenPadding,
    backgroundColor: properties.expansionTileBackgroundColor,
    collapsedBackgroundColor: properties.expansionTileCollapsedBackgroundColor,
    textColor: properties.expansionTileTextColor,
    collapsedTextColor: properties.expansionTileCollapsedTextColor,
    iconColor: properties.expansionTileIconColor,
    collapsedIconColor: properties.expansionTileCollapsedIconColor,
    controlAffinity: properties.expansionTileControlAffinity,
    controller: properties.expansionTileController,
    onExpansionChanged: properties.expansionTileOnExpansionChanged,
  );
  return Eval.pure(IrNativeValue(Value(expansionTileWidget)));
}
