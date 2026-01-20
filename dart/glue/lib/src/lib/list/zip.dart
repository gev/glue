import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Zip function - combines two lists element-wise into pairs
/// Mirrors Haskell Glue.Lib.List.Zip.zip exactly
Ir zip = IrNativeFunc(zipImpl);

/// Zip function implementation
/// Mirrors Haskell Glue.Lib.List.Zip.zipImpl exactly
Eval<Ir> zipImpl(Ir list1Ir) {
  return Eval.pure(IrNativeFunc(zipWith(list1Ir)));
}

/// Helper function for second list argument
/// Mirrors Haskell Glue.Lib.List.Zip.zipWith exactly
Eval<Ir> Function(Ir) zipWith(Ir list1Ir) {
  return (Ir list2Ir) {
    return sequenceAll([eval(list1Ir), eval(list2Ir)]).flatMap((evaluated) {
      return switch (evaluated) {
        [final list1, final list2] =>
          list1 is IrList && list2 is IrList
              ? Eval.pure(
                  IrList(
                    zipLists(list1.elements.toList(), list2.elements.toList()),
                  ),
                )
              : throwError(wrongArgumentType(['list', 'list'])),
        _ => throwError(wrongArgumentType(['list', 'list'])),
      };
    });
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
