import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Foldr function - strict right-to-left fold
Ir foldr = IrNativeFunc(foldrImpl);

Eval<Ir> foldrImpl(Ir funcIr) {
  return Eval.pure(IrNativeFunc(foldrInit(funcIr)));
}

Eval<Ir> Function(Ir) foldrInit(Ir funcIr) {
  return (Ir initValIr) {
    return Eval.pure(IrNativeFunc(foldrOver(funcIr, initValIr)));
  };
}

Eval<Ir> Function(Ir) foldrOver(Ir funcIr, Ir initValIr) {
  return (Ir listIr) {
    return switch (listIr) {
      IrList(elements: final elements) => _foldrLoop(
        funcIr,
        initValIr,
        elements.unlock,
      ),
      _ => throwError(wrongArgumentType(['function', 'any', 'list'])),
    };
  };
}

Eval<Ir> _foldrLoop(Ir funcIr, Ir acc, List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure(acc);
  }
  // Берем последний элемент, свертаем хвост, а затем применяем f(x, acc)
  final last = elements.last;
  final rest = elements.sublist(0, elements.length - 1);

  return _foldrLoop(funcIr, acc, rest).bind((newAcc) {
    return apply(funcIr, [last, newAcc]);
  });
}
