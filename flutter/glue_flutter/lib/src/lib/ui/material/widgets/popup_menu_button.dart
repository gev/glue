import 'package:flutter/material.dart';
import 'package:glue/error.dart';
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
  final itemBuilder = properties
      .getValue<List<PopupMenuEntry<dynamic>> Function(BuildContext)>(
        'item-builder',
      );
  if (itemBuilder == null) {
    return throwError(
      wrongArgumentType([
        '  :item-builder must be provided and be a function that returns a list of PopupMenuEntry',
      ]),
    );
  }
  return getRuntime().map((runtime) {
    final popupMenuButtonWidget = PopupMenuButton<dynamic>(
      key: properties.key,
      itemBuilder: itemBuilder,
      initialValue: properties.getValue<dynamic>('initial-value'),
      onSelected: properties.getValue<Function(dynamic)>('on-selected'),
      onCanceled: properties.getVoidCallback('on-canceled')?.call(runtime),
      tooltip: properties.getString('tooltip'),
      elevation: properties.getDouble('elevation'),
      padding:
          properties.getValue<EdgeInsetsGeometry>('padding') ??
          const EdgeInsets.all(8.0),
      splashRadius: properties.getDouble('splash-radius'),
      icon: properties.getWidget('icon'),
      iconSize: properties.getDouble('icon-size'),
      offset: properties.getValue<Offset>('offset') ?? Offset.zero,
      enabled: properties.getBool('enabled') ?? true,
      shape: properties.getValue<ShapeBorder>('shape'),
      color: properties.getColor('color'),
      enableFeedback: properties.getBool('enable-feedback'),
      constraints: properties.getValue<BoxConstraints>('constraints'),
      position: properties.getValue<PopupMenuPosition>('position'),
      child: properties.child,
    );
    return IrNativeValue(Value(popupMenuButtonWidget));
  });
}
