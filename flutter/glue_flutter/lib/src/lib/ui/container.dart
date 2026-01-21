import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue_flutter/src/widgets/glue_container.dart';

/// Container widget function
/// Creates Flutter Column/Row from Glue (container props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object (children in props)
Eval<Ir> containerImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final containerWidget = GlueContainer(properties: props.properties.unlock);
  return Eval.pure(IrNativeValue(HostValue(containerWidget)));
}
