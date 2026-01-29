import 'package:flutter/widgets.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding all function - (padding-all 10)
final paddingAll = IrNativeFunc(paddingAllImpl);

Eval<Ir> paddingAllImpl(Ir value) {
  final padding = extractDouble(value);
  if (padding == null) {
    return throwError(wrongArgumentType(['number']));
  }
  return createPadding(EdgeInsets.all(padding));
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}
