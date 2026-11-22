import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter_ui/utils/widget_properties.dart';
import 'package:material_ui/material_ui.dart';

/// OutlineInputBorder function
/// Creates Flutter OutlineInputBorder from Glue expressions
final Ir outlineInputBorder = IrNativeFunc(outlineInputBorderImpl);

/// OutlineInputBorder implementation - takes properties object
Eval<Ir> outlineInputBorderImpl(Ir props) =>
    _createOutlineInputBorder(WidgetProperties(props));

/// Create OutlineInputBorder from properties
Eval<Ir> _createOutlineInputBorder(WidgetProperties properties) {
  final border = OutlineInputBorder(
    borderSide:
        properties.getValue<BorderSide>('border-side') ?? const BorderSide(),
    borderRadius:
        properties.getValue<BorderRadius>('border-radius') ??
        const BorderRadius.all(Radius.circular(4.0)),
    gapPadding: properties.getDouble('gap-padding') ?? 4.0,
  );
  return Eval.pure(IrNativeValue(Value(border)));
}
