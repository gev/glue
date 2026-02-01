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
  return getRuntime().map((runtime) {
    final expansionTileWidget = ExpansionTile(
      key: properties.key,
      leading: properties.getWidget('leading'),
      title: properties.child ?? const SizedBox(),
      subtitle: properties.getWidget('subtitle'),
      trailing: properties.getWidget('trailing'),
      children: properties.getWidgets('children'),
      initiallyExpanded: properties.getBool('initially-expanded') ?? false,
      maintainState: properties.getBool('maintain-state') ?? false,
      tilePadding: properties.getValue<>('tile-padding'),
      expandedAlignment: properties.getValue<>('expanded-alignment'),
      expandedCrossAxisAlignment: properties.getValue<>(
        'expanded-cross-axis-alignment',
      ),
      childrenPadding: properties.getValue<>('children-padding'),
      backgroundColor: properties.getColor('background-color'),
      collapsedBackgroundColor: properties.getColor(
        'collapsed-background-color',
      ),
      textColor: properties.getColor('text-color'),
      collapsedTextColor: properties.getColor('collapsed-text-color'),
      iconColor: properties.getColor('icon-color'),
      collapsedIconColor: properties.getColor('collapsed-icon-color'),
      controlAffinity: properties.getValue<>('control-affinity'),
      controller: properties.getValue<>('controller'),
      onExpansionChanged: properties.getValue<>('on-expansion-changed'),
    );
    return IrNativeValue(Value(expansionTileWidget));
  });
}
