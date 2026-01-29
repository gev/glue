import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/material_properties.dart';

/// UserAccountsDrawerHeader widget function
/// Creates Flutter UserAccountsDrawerHeader from Glue (user-accounts-drawer-header props) expressions
final Ir userAccountsDrawerHeader = IrNativeFunc(userAccountsDrawerHeaderImpl);

/// UserAccountsDrawerHeader implementation - takes properties object
Eval<Ir> userAccountsDrawerHeaderImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createUserAccountsDrawerHeader(
    MaterialProperties(properties.unlock),
  ),
  _ => _createUserAccountsDrawerHeader(MaterialProperties.empty()),
};

/// Create UserAccountsDrawerHeader widget from properties
Eval<Ir> _createUserAccountsDrawerHeader(MaterialProperties properties) {
  final userAccountsDrawerHeaderWidget = UserAccountsDrawerHeader(
    decoration: properties.userAccountsDrawerHeaderDecoration,
    margin: properties.userAccountsDrawerHeaderMargin,
    currentAccountPicture:
        properties.userAccountsDrawerHeaderCurrentAccountPicture,
    otherAccountsPictures:
        properties.userAccountsDrawerHeaderOtherAccountsPictures,
    accountName: properties.userAccountsDrawerHeaderAccountName,
    accountEmail: properties.userAccountsDrawerHeaderAccountEmail,
    onDetailsPressed: properties.userAccountsDrawerHeaderOnDetailsPressed,
    arrowColor: properties.userAccountsDrawerHeaderArrowColor ?? Colors.white,
  );
  return Eval.pure(IrNativeValue(Value(userAccountsDrawerHeaderWidget)));
}
