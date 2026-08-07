import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Strict left-to-right fold over a list.
///
/// Curried signature: `foldl(func)(initVal)(list)`
///
/// Arguments:
/// - [func]: The accumulator function applied at each step.
/// - [initVal]: The initial value of the accumulator.
/// - [list]: The target [IrList] to fold over.
///
/// Lambda argument order: `(acc, x)`
/// - [acc]: The current accumulator value (first parameter).
/// - [x]: The current element of the list (second parameter).
Ir foldl = IrNativeFunc(foldlImpl);

Eval<Ir> foldlImpl(Ir funcIr) {
  return Eval.pure(IrNativeFunc(foldlInit(funcIr)));
}

Eval<Ir> Function(Ir) foldlInit(Ir funcIr) {
  return (Ir initValIr) {
    return Eval.pure(IrNativeFunc(foldlOver(funcIr, initValIr)));
  };
}

Eval<Ir> Function(Ir) foldlOver(Ir funcIr, Ir initValIr) {
  return (Ir listIr) {
    return switch (listIr) {
      IrList(elements: final elements) => _foldlLoop(
        funcIr,
        initValIr,
        elements.unlock,
      ),
      _ => throwError(wrongArgumentType(['function', 'any', 'list'])),
    };
  };
}

Eval<Ir> _foldlLoop(Ir funcIr, Ir acc, List<Ir> elements) {
  if (elements.isEmpty) {
    return Eval.pure(acc);
  }
  // Передаем аккумулятор и текущий элемент в функцию: f(acc, x)
  return apply(funcIr, [acc, elements.first]).bind((newAcc) {
    return _foldlLoop(funcIr, newAcc, elements.sublist(1));
  });
}
