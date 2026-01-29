import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// PopupMenuButton widget function
/// Creates Flutter PopupMenuButton from Glue (popup-menu-button props) expressions
final Ir popupMenuButton = IrNativeFunc(popupMenuButtonImpl);

/// PopupMenuButton implementation - takes properties object
Eval<Ir> popupMenuButtonImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createPopupMenuButton(
    MaterialProperties(properties.unlock),
  ),
  _ => _createPopupMenuButton(MaterialProperties.empty()),
};

/// Create PopupMenuButton widget from properties
Eval<Ir> _createPopupMenuButton(MaterialProperties properties) {
  final popupMenuButtonWidget = PopupMenuButton<Object>(
    itemBuilder: properties.popupMenuItemBuilder ?? (context) => [],
    initialValue: properties.popupMenuInitialValue,
    onSelected: properties.popupMenuOnSelected,
    onCanceled: properties.popupMenuOnCanceled,
    tooltip: properties.popupMenuTooltip,
    elevation: properties.popupMenuElevation,
    padding: properties.popupMenuPadding,
    child: properties.popupMenuChild,
    splashRadius: properties.popupMenuSplashRadius,
    icon: properties.popupMenuIcon,
    iconSize: properties.popupMenuIconSize,
    offset: properties.popupMenuOffset,
    enabled: properties.popupMenuEnabled,
    shape: properties.popupMenuShape,
    color: properties.popupMenuColor,
    enableFeedback: properties.popupMenuEnableFeedback,
    constraints: properties.popupMenuConstraints,
    position: properties.popupMenuPosition,
  );
  return Eval.pure(IrNativeValue(Value(popupMenuButtonWidget)));
}
