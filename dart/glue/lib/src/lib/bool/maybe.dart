import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

final Ir maybe = IrSpecial(maybeImpl);

Eval<Ir> maybeImpl(List<Ir> args) {
  return switch (args) {
    [final f, final x] => eval(x).bind((x_) {
      return isExist(x_)
          ? eval(f).bind((g) => apply(g, [x_]))
          : Eval.pure(IrVoid());
    }),
    _ => throwError(wrongNumberOfArguments()),
  };
}
