import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

final Ir and_ = IrSpecial(andImpl);

Eval<Ir> andImpl(List<Ir> args) {
  return switch (args) {
    [final a, final b] => eval(a).bind((a_) {
      return isFalsy(a_)
          ? Eval.pure(IrBool(false))
          : eval(b).bind((v) => Eval.pure(IrBool(isTruthy(v))));
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
