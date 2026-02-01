import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// PopupMenuButton widget function
/// Creates Flutter PopupMenuButton from Glue (popup-menu-button props) expressions
final Ir popupMenuButton = IrNativeFunc(popupMenuButtonImpl);

/// PopupMenuButton implementation - takes properties object
Eval<Ir> popupMenuButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPopupMenuButton(
    WidgetProperties(properties.unlock),
  ),
  _ => _createPopupMenuButton(WidgetProperties.empty()),
};

/// Create PopupMenuButton widget from properties
Eval<Ir> _createPopupMenuButton(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final popupMenuButtonWidget = PopupMenuButton<dynamic>(
      key: properties.key,
      itemBuilder: properties.getValue<>('item-builder') ?? (context) => [],
      initialValue: properties.getValue<>('initial-value'),
      onSelected: properties.getValue<>('on-selected'),
      onCanceled: properties.getVoidCallback('on-canceled', runtime),
      tooltip: properties.getString('tooltip'),
      elevation: properties.getDouble('elevation'),
      padding: properties.getValue<>('padding'),
      splashRadius: properties.getDouble('splash-radius'),
      icon: properties.getWidget('icon'),
      iconSize: properties.getDouble('icon-size'),
      offset: properties.getValue<>('offset') ?? Offset.zero,
      enabled: properties.getBool('enabled') ?? true,
      shape: properties.getValue<>('shape'),
      color: properties.getColor('color'),
      enableFeedback: properties.getBool('enable-feedback'),
      constraints: properties.getValue<>('constraints'),
      position: properties.getValue<>('position'),
      child: properties.child,
    );
    return IrNativeValue(Value(popupMenuButtonWidget));
  });
}
