import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding only function - (padding-only (:left 10 :top 5 :right 10 :bottom 5))
final paddingOnly = IrNativeFunc(paddingOnlyImpl);

Eval<Ir> paddingOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingOnly(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPaddingOnly(dynamic properties) {
  final props = (properties as IMap<String, Ir>).unlock as Map<String, dynamic>;
  final top = extractDouble(props['top']) ?? 0;
  final right = extractDouble(props['right']) ?? 0;
  final bottom = extractDouble(props['bottom']) ?? 0;
  final left = extractDouble(props['left']) ?? 0;
  return createPadding(
    EdgeInsets.only(top: top, left: left, bottom: bottom, right: right),
  );
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(HostValue(insets)));
}
