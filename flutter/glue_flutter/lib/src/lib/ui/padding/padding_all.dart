import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding all function - (padding-all 10)
final paddingAll = IrNativeFunc(paddingAllImpl);

Eval<Ir> paddingAllImpl(Ir value) => switch (value) {
  IrFloat(value: final val) => createPadding(EdgeInsets.all(val)),
  IrInteger(value: final val) => createPadding(EdgeInsets.all(val.toDouble())),
  _ => throwError(wrongArgumentType(['number'])),
};

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(HostValue(insets)));
}
