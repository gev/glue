import 'package:flutter/material.dart';
import 'package:fast_immutable_collections/fast_immutable_collections.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Padding functions - create EdgeInsets objects
final paddingAll = IrNativeFunc(paddingAllImpl);
final paddingSymmetric = IrNativeFunc(paddingSymmetricImpl);
final paddingOnly = IrNativeFunc(paddingOnlyImpl);
final paddingDirectional = IrNativeFunc(paddingDirectionalImpl);

Eval<Ir> paddingAllImpl(Ir value) => switch (value) {
  IrFloat(value: final val) => createPadding(EdgeInsets.all(val)),
  IrInteger(value: final val) => createPadding(EdgeInsets.all(val.toDouble())),
  _ => throwError(wrongArgumentType(['number'])),
};

Eval<Ir> paddingSymmetricImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingSymmetric(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> paddingOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingOnly(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> paddingDirectionalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createPaddingDirectional(properties),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(HostValue(insets)));
}

Eval<Ir> createPaddingSymmetric(dynamic properties) {
  final props = (properties as IMap<String, Ir>).unlock as Map<String, dynamic>;
  final vertical = extractDouble(props['vertical']) ?? 0;
  final horizontal = extractDouble(props['horizontal']) ?? 0;
  return createPadding(
    EdgeInsets.symmetric(vertical: vertical, horizontal: horizontal),
  );
}

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
