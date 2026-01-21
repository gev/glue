import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';

/// Parse edge insets from Glue IR value
/// Supports single value (all), two values (vertical, horizontal), four values (top, right, bottom, left)
EdgeInsets? parseEdgeInsets(Ir ir) {
  return switch (ir) {
    IrInteger(value: final val) => EdgeInsets.all(val.toDouble()),
    IrFloat(value: final val) => EdgeInsets.all(val),
    IrList(elements: final elements) => _parseEdgeInsetsFromList(
      elements.unlock,
    ),
    _ => null,
  };
}

/// Parse edge insets from list
EdgeInsets? _parseEdgeInsetsFromList(List<Ir> elements) {
  return switch (elements.length) {
    1 => switch (elements[0]) {
      IrInteger(value: final val) => EdgeInsets.all(val.toDouble()),
      IrFloat(value: final val) => EdgeInsets.all(val),
      _ => null,
    },
    2 => switch ((elements[0], elements[1])) {
      (IrInteger(value: final v), IrInteger(value: final h)) =>
        EdgeInsets.symmetric(vertical: v.toDouble(), horizontal: h.toDouble()),
      (IrFloat(value: final v), IrFloat(value: final h)) =>
        EdgeInsets.symmetric(vertical: v, horizontal: h),
      _ => null,
    },
    4 => switch ((elements[0], elements[1], elements[2], elements[3])) {
      (
        IrInteger(value: final t),
        IrInteger(value: final r),
        IrInteger(value: final b),
        IrInteger(value: final l),
      ) =>
        EdgeInsets.fromLTRB(
          t.toDouble(),
          r.toDouble(),
          b.toDouble(),
          l.toDouble(),
        ),
      (
        IrFloat(value: final t),
        IrFloat(value: final r),
        IrFloat(value: final b),
        IrFloat(value: final l),
      ) =>
        EdgeInsets.fromLTRB(t, r, b, l),
      _ => null,
    },
    _ => null,
  };
}
