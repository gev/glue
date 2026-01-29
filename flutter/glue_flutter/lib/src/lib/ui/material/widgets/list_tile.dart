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
  final listTileWidget = ListTile(
    leading: properties.child, // leading widget
    title: properties.title,
    subtitle: properties.subtitle,
    trailing: properties.trailing,
    isThreeLine: properties.isThreeLine ?? false,
    dense: properties.dense,
    visualDensity: properties.visualDensity,
    shape: properties.shape,
    style: properties.listTileStyle,
    selectedColor: properties.selectedColor,
    iconColor: properties.iconColor,
    textColor: properties.textColor,
    titleTextStyle: properties.listTileTitleTextStyle,
    subtitleTextStyle: properties.subtitleTextStyle,
    leadingAndTrailingTextStyle: properties.leadingAndTrailingTextStyle,
    contentPadding: properties.contentPadding,
    enabled: properties.enabled ?? true,
    onTap: properties.onTileTap,
    onLongPress: properties.onTileLongPress,
    onFocusChange: properties.onFocusChange,
    mouseCursor: properties.mouseCursor,
    selected: properties.selected ?? false,
    focusColor: properties.focusColor,
    hoverColor: properties.hoverColor,
    splashColor: properties.splashColor,
    focusNode: properties.focusNode,
    autofocus: properties.autofocus,
    tileColor: properties.tileColor,
    selectedTileColor: properties.selectedTileColor,
    enableFeedback: properties.enableFeedback ?? true,
    horizontalTitleGap: properties.horizontalTitleGap,
    minVerticalPadding: properties.minVerticalPadding,
    minLeadingWidth: properties.minLeadingWidth,
    minTileHeight: properties.minTileHeight,
    titleAlignment: properties.titleAlignment,
  );
  return Eval.pure(IrNativeValue(Value(listTileWidget)));
}
