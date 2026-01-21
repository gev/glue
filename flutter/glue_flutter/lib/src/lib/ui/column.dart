import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_column.dart';

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final columnWidget = GlueColumn(properties: props.properties.unlock);
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}
