import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

final Ir inkWell = IrNativeFunc(inkWellImpl);

Eval<Ir> inkWellImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createInkWell(
    WidgetProperties(properties.unlock),
  ),
  _ => _createInkWell(WidgetProperties.empty()),
};

Eval<Ir> _createInkWell(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final inkWellWidget = InkWell(
      key: properties.key,
      onTap: properties.getVoidCallback('on-tap')?.call(runtime),
      onLongPress: properties.getVoidCallback('on-long-press')?.call(runtime),
      onHover: properties.getCallback<bool>('on-hover')?.call(runtime),
      onFocusChange: properties
          .getCallback<bool>('on-focus-change')
          ?.call(runtime),
      focusNode: properties.getValue<FocusNode>('focus-node'),
      autofocus: properties.getBool('autofocus') ?? false,
      borderRadius: properties.getValue<BorderRadius>('clip-behavior'),
      child: properties.child,
    );
    return IrNativeValue(Value(inkWellWidget));
  });
}
