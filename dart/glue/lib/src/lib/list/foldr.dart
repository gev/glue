import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Strict right-to-left fold over a list.
///
/// Curried signature: `foldr(func)(initVal)(list)`
///
/// Arguments:
/// - [func]: The accumulator function applied at each step.
/// - [initVal]: The initial value of the accumulator.
/// - [list]: The target [IrList] to fold over.
///
/// Lambda argument order: `(x, acc)`
/// - [x]: The current element of the list (first parameter).
/// - [acc]: The current accumulator value (second parameter).
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
