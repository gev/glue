import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// UserAccountsDrawerHeader widget function
/// Creates Flutter UserAccountsDrawerHeader from Glue (user-accounts-drawer-header props) expressions
final Ir userAccountsDrawerHeader = IrNativeFunc(userAccountsDrawerHeaderImpl);

/// UserAccountsDrawerHeader implementation - takes properties object
Eval<Ir> userAccountsDrawerHeaderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createUserAccountsDrawerHeader(
    WidgetProperties(properties.unlock),
  ),
  _ => _createUserAccountsDrawerHeader(WidgetProperties.empty()),
};

/// Create UserAccountsDrawerHeader widget from properties
Eval<Ir> _createUserAccountsDrawerHeader(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final userAccountsDrawerHeaderWidget = UserAccountsDrawerHeader(
      key: properties.key,
      decoration: properties.getValue<>('decoration'),
      margin: properties.getValue<>('margin'),
      currentAccountPicture: properties.getWidget('current-account-picture'),
      otherAccountsPictures: properties.getWidgets('other-accounts-pictures'),
      accountName: properties.getWidget('account-name'),
      accountEmail: properties.getWidget('account-email'),
      onDetailsPressed: properties.getVoidCallback(
        'on-details-pressed',
        runtime,
      ),
      arrowColor: properties.getColor('arrow-color') ?? Colors.white,
    );
    return IrNativeValue(Value(userAccountsDrawerHeaderWidget));
  });
}
