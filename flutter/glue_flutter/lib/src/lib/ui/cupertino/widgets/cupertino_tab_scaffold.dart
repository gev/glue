import 'package:flutter/cupertino.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// CupertinoTabScaffold widget function
/// Creates Flutter CupertinoTabScaffold from Glue expressions
/// Expects keyword arguments: :tab-bar, :tab-builder, :controller, :background-color, :resize-to-avoid-bottom-inset, :restoration-id
final Ir cupertinoTabScaffold = IrNativeFunc(cupertinoTabScaffoldImpl);

/// CupertinoTabScaffold implementation - takes properties object with keyword arguments
Eval<Ir> cupertinoTabScaffoldImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createCupertinoTabScaffold(
    WidgetProperties(properties.unlock),
  ),
  _ => _createCupertinoTabScaffold(WidgetProperties.empty()),
};

/// Create CupertinoTabScaffold widget from properties object
Eval<Ir> _createCupertinoTabScaffold(WidgetProperties properties) {
  final tabBar = properties.getValue<CupertinoTabBar>('tab-bar');
  if (tabBar == null) {
    return throwError(wrongArgumentType(['tab-bar required']));
  }
  final tabBuilder = properties.getValue<IndexedWidgetBuilder>('tab-builder');
  if (tabBuilder == null) {
    return throwError(wrongArgumentType(['tab-builder required']));
  }
  final widget = CupertinoTabScaffold(
    key: properties.key,
    tabBar: tabBar,
    tabBuilder: tabBuilder,
    controller: properties.getValue<CupertinoTabController>('controller'),
    backgroundColor: properties.getColor('background-color'),
    resizeToAvoidBottomInset:
        properties.getBool('resize-to-avoid-bottom-inset') ?? true,
    restorationId: properties.getString('restoration-id'),
  );
  return Eval.pure(IrNativeValue(Value(widget)));
}
