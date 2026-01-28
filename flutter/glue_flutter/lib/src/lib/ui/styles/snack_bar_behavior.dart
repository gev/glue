import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// SnackBarBehavior enum object
/// Represents all SnackBarBehavior values as Glue object properties
final snackBarBehavior = IrObject({
  'fixed': IrNativeValue(Value(SnackBarBehavior.fixed)),
  'floating': IrNativeValue(Value(SnackBarBehavior.floating)),
});
