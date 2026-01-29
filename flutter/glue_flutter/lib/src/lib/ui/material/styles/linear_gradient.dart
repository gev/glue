import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

/// LinearGradient function - (linearGradient colors stops begin end)
/// Creates a linear gradient (simplified version)
final linearGradientFunc = IrNativeFunc(linearGradientImpl);

Eval<Ir> linearGradientImpl(Ir colors) =>
    Eval.pure(IrNativeFunc((Ir stops) => createLinearGradient(colors, stops)));

Eval<Ir> createLinearGradient(Ir colors, Ir stops) {
  // This would need more complex implementation for a full gradient
  // For now, return a placeholder
  return Eval.pure(
    IrString('Linear gradient function - implementation needed'),
  );
}
