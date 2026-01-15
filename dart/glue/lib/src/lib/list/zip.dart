import '../../eval.dart';
import '../../eval/exception.dart';
import '../../ir.dart';

/// Zip function - combines two lists element-wise into pairs
/// Mirrors Haskell Glue.Lib.List.Zip.zip exactly
Eval<Ir> zip(List<Ir> args) {
  return switch (args) {
    [final list1Ir, final list2Ir] =>
      sequenceAll([eval(list1Ir), eval(list2Ir)]).flatMap((evaluated) {
        final list1 = evaluated[0];
        final list2 = evaluated[1];
        if (list1 is IrList && list2 is IrList) {
          final zipped = zipLists(
            list1.elements.toList(),
            list2.elements.toList(),
          );
          return Eval.pure(IrList(zipped));
        } else {
          return throwError(wrongArgumentType(['list', 'list']));
        }
      }),
    _ => throwError(wrongNumberOfArguments()),
  };
}

/// Helper function to zip two lists
List<Ir> zipLists(List<Ir> list1, List<Ir> list2) {
  if (list1.isEmpty || list2.isEmpty) {
    return [];
  }
  final pair = IrList([list1[0], list2[0]]);
  return [pair, ...zipLists(list1.sublist(1), list2.sublist(1))];
}
