import 'package:flutter/material.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ListTile widget function
/// Creates Flutter ListTile from Glue (list-tile props) expressions
final Ir listTile = IrNativeFunc(listTileImpl);

/// ListTile implementation - takes properties object
Eval<Ir> listTileImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createListTile(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

/// Create ListTile widget from properties
Eval<Ir> _createListTile(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final listTileWidget = ListTile(
      key: properties.key,
      leading: properties.getWidget('leading'),
      title: properties.getWidget('title'),
      subtitle: properties.getWidget('subtitle'),
      trailing: properties.getWidget('trailing'),
      isThreeLine: properties.getBool('is-three-line') ?? false,
      dense: properties.getBool('dense'),
      visualDensity: properties.getValue('visual-density'),
      shape: properties.getValue('shape'),
      style: properties.getValue('style'),
      selectedColor: properties.getColor('selected-color'),
      iconColor: properties.getColor('icon-color'),
      textColor: properties.getColor('text-color'),
      titleTextStyle: properties.getValue('title-text-style'),
      subtitleTextStyle: properties.getValue('subtitle-text-style'),
      leadingAndTrailingTextStyle: properties.getValue(
        'leading-and-trailing-text-style',
      ),
      contentPadding: properties.getValue('content-padding'),
      enabled: properties.getBool('enabled') ?? true,
      onTap: properties.getVoidCallback('on-tap', runtime),
      onLongPress: properties.getVoidCallback('on-long-press', runtime),
      onFocusChange: properties.getValue('on-focus-change'),
      mouseCursor: properties.getValue('mouse-cursor'),
      selected: properties.getBool('selected') ?? false,
      focusColor: properties.getColor('focus-color'),
      hoverColor: properties.getColor('hover-color'),
      splashColor: properties.getColor('splash-color'),
      focusNode: properties.getValue('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      tileColor: properties.getColor('tile-color'),
      selectedTileColor: properties.getColor('selected-tile-color'),
      enableFeedback: properties.getBool('enable-feedback') ?? true,
      horizontalTitleGap: properties.getDouble('horizontal-title-gap'),
      minVerticalPadding: properties.getDouble('min-vertical-padding'),
      minLeadingWidth: properties.getDouble('min-leading-width'),
      minTileHeight: properties.getDouble('min-tile-height'),
      titleAlignment: properties.getValue('title-alignment'),
      internalAddSemanticForOnTap:
          properties.getBool('internal-add-semantic-for-on-tap') ?? false,
      statesController: properties.getValue('states-controller'),
    );
    return IrNativeValue(Value(listTileWidget));
  });
}
