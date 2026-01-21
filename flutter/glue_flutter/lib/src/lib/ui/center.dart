import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import '../../widgets/glue_center.dart';

/// Center widget function
/// Creates Flutter Center from Glue (center child) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes child
Eval<Ir> centerImpl(Ir child) {
  if (child is! IrNativeValue) {
    return throwError(wrongArgumentType(['widget']));
  }

  final centerWidget = GlueCenter(child);
  return Eval.pure(IrNativeValue(HostValue(centerWidget)));
}
