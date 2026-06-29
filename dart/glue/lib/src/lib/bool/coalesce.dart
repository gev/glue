import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

final Ir coalesce = IrSpecial(coalesceImpl);

Eval<Ir> coalesceImpl(List<Ir> args) {
  return switch (args) {
    [final a, final b] => eval(a).bind((a_) {
      return isExist(a_) ? Eval.pure(a_) : eval(b);
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
