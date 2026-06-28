import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'package:glue_flutter/src/utils/value_extractors.dart';

/// Edge insets all function - (edge-insetsall 10)
final edgeInsetsAll = IrNativeFunc(edgeInsetsAllImpl);

Eval<Ir> edgeInsetsAllImpl(Ir value) {
  final edgeInsets = extractDouble(value);
  if (edgeInsets == null) {
    return throwError(wrongArgumentType(['number']));
  }
  return createEdgeInsetsAll(EdgeInsets.all(edgeInsets));
}

Eval<Ir> createEdgeInsetsAll(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(Value(insets)));
}

/// Edge insets directional function - (edge-insetsdirectional (:start 10 :top 5 :end 10 :bottom 5))
final edgeInsetsDirectional = IrNativeFunc(edgeInsetsDirectionalImpl);

Eval<Ir> edgeInsetsDirectionalImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createEdgeInsetsDirectional(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createEdgeInsetsDirectional(WidgetProperties properties) {
  final insets = EdgeInsetsDirectional.only(
    start: properties.start ?? 0,
    top: properties.top ?? 0,
    end: properties.end ?? 0,
    bottom: properties.bottom ?? 0,
  );
  return Eval.pure(IrNativeValue(Value(insets)));
}

/// Edge insets only function - (edge-insetsonly (:left 10 :top 5 :right 10 :bottom 5))
final edgeInsetsOnly = IrNativeFunc(edgeInsetsOnlyImpl);

Eval<Ir> edgeInsetsOnlyImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createEdgeInsetsOnly(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createEdgeInsetsOnly(WidgetProperties properties) {
  final insets = EdgeInsets.only(
    top: properties.top ?? 0,
    left: properties.left ?? 0,
    bottom: properties.bottom ?? 0,
    right: properties.right ?? 0,
  );
  return Eval.pure(IrNativeValue(Value(insets)));
}

/// Edge insets symmetric function - (edge-insetssymmetric (:vertical 10 :horizontal 5))
final edgeInsetsSymmetric = IrNativeFunc(edgeInsetsSymmetricImpl);

Eval<Ir> edgeInsetsSymmetricImpl(Ir props) => switch (props) {
  IrObject(:final properties) => createEdgeInsetsSymmetric(
    WidgetProperties(properties.unlock),
  ),
  _ => throwError(wrongArgumentType(['object'])),
};

Eval<Ir> createEdgeInsetsSymmetric(WidgetProperties properties) {
  final insets = EdgeInsets.symmetric(
    vertical: properties.vertical ?? 0,
    horizontal: properties.horizontal ?? 0,
  );
  return Eval.pure(IrNativeValue(Value(insets)));
}
