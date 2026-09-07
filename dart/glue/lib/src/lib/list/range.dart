import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Range special form
/// Mirrors Haskell Glue.Lib.List.Range.range exactly
final Ir range = IrSpecial(rangeImpl);

/// Range special form implementation
/// Mirrors Haskell Glue.Lib.List.Range.rangeImpl exactly
Eval<Ir> rangeImpl(List<Ir> rawArgs) {
  return sequenceAll(rawArgs.map(eval).toList()).bind((args) {
    return switch (args) {
      [IrInteger(value: final n)] => Eval.pure(
        IrList(
          n > 0
              ? [for (int i = 0; i < n; i++) IrInteger(i)]
              : [for (int i = 0; i >= n + 1; i--) IrInteger(i)],
        ),
      ),
      [IrInteger(value: final start), IrInteger(value: final stop)] =>
        Eval.pure(
          IrList(
            stop > start
                ? [for (int i = start; i < stop; i++) IrInteger(i)]
                : [for (int i = start; i >= stop + 1; i--) IrInteger(i)],
          ),
        ),
      [
        IrInteger(value: final start),
        IrInteger(value: final stop),
        IrInteger(value: final step),
      ] =>
        step == 0
            ? throwError(wrongArgumentType(["Step shouldn't be zero"]))
            : Eval.pure(
                IrList(
                  stop > start
                      ? [
                          for (int i = start; i <= stop - 1; i += step)
                            IrInteger(i),
                        ]
                      : [
                          for (int i = start; i >= stop + 1; i += step)
                            IrInteger(i),
                        ],
                ),
              ),
      _ => throwError(
        wrongArgumentType([
          "Integer n required",
          "Integer `start` and Integer `stop` and optional Integer `step` required",
        ]),
      ),
    };
  });
}
