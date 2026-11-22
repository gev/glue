import 'package:glue/ir.dart';
import 'package:material_ui/material_ui.dart';

/// SnackBarBehavior enum object
/// Represents all SnackBarBehavior values as Glue object properties
final snackBarBehavior = IrObject({
  'fixed': IrNativeValue(Value(SnackBarBehavior.fixed)),
  'floating': IrNativeValue(Value(SnackBarBehavior.floating)),
});
