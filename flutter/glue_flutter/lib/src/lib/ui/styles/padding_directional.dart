import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding directional function - (padding-directional (:start 10 :top 5 :end 10 :bottom 5))
final paddingDirectional = IrNativeFunc(paddingDirectionalImpl);

Eval<Ir> paddingDirectionalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingDirectional(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingDirectional(dynamic properties) {
  final props = (properties as IMap<String, Ir>).unlock as Map<String, dynamic>;
  final top = extractDouble(props['top']) ?? 0;
  final start = extractDouble(props['start']) ?? 0;
  final bottom = extractDouble(props['bottom']) ?? 0;
  final end = extractDouble(props['end']) ?? 0;
  return createPadding(
    EdgeInsetsDirectional.only(
      start: start,
      top: top,
      end: end,
      bottom: bottom,
    ),
  );
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}
