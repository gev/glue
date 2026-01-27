import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding symmetric function - (padding-symmetric (:vertical 10 :horizontal 5))
final paddingSymmetric = IrNativeFunc(paddingSymmetricImpl);

Eval<Ir> paddingSymmetricImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingSymmetric(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingSymmetric(dynamic properties) {
  final props = (properties as IMap<String, Ir>).unlock as Map<String, dynamic>;
  final vertical = extractDouble(props['vertical']) ?? 0;
  final horizontal = extractDouble(props['horizontal']) ?? 0;
  return createPadding(
    EdgeInsets.symmetric(vertical: vertical, horizontal: horizontal),
  );
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}
