import 'dart:ui';
import 'package:flutter/widgets.dart';
import 'package:glue/error.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// ImageFiltered widget function
/// Creates Flutter ImageFiltered from Glue expressions
final Ir imageFiltered = IrNativeFunc((Ir props) {
  final properties = WidgetProperties(props);
  final filter = properties.getValue<ImageFilter>('image-filter');
  if (filter == null) {
    return throwError(
      wrongArgumentType(['ImageFilter property `image-filter` required']),
    );
  }
  final imageFilteredWidget = ImageFiltered(
    key: properties.key,
    imageFilter: filter,
    child: properties.child,
  );
  return Eval.pure(IrNativeValue(Value(imageFilteredWidget)));
});
