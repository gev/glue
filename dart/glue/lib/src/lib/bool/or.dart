import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

final Ir or_ = IrSpecial(orImpl);

Eval<Ir> orImpl(List<Ir> args) {
  return switch (args) {
    [final a, final b] => eval(a).bind((a_) {
      return isTruthy(a_)
          ? Eval.pure(IrBool(true))
          : eval(b).bind((v) => Eval.pure(IrBool(isTruthy(v))));
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
