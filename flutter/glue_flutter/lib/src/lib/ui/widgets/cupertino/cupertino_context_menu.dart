import 'package:flutter/cupertino.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoContextMenu widget function
/// Creates Flutter CupertinoContextMenu from Glue expressions
/// Expects keyword arguments: :child, :actions, etc.
final Ir cupertinoContextMenu = IrNativeFunc(cupertinoContextMenuImpl);

/// CupertinoContextMenu implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoContextMenuImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoContextMenu(
    Properties(properties.unlock),
  ),
  _ => _createCupertinoContextMenu(Properties.empty()),
};

/// Create CupertinoContextMenu widget from properties object
Eval<Ir> _createCupertinoContextMenu(Properties properties) {
  return getRuntime().map((runtime) {
    final contextMenuWidget = CupertinoContextMenu(
      actions: properties.cupertinoContextMenuActions,
      child: properties.child ?? const Text('Context Menu'),
    );
    return IrNativeValue(Value(contextMenuWidget));
  });
}
