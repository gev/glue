import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// ZipWith function - combines two lists element-wise using a given function
/// Mirrors Haskell Glue.Lib.List.ZipWith.zipWith exactly
Ir zipWith = IrNativeFunc(zipWithImpl);

/// ZipWith function implementation
/// Mirrors Haskell Glue.Lib.List.ZipWith.zipWithImpl exactly
Eval<Ir> zipWithImpl(Ir funcIr) {
  return Eval.pure(IrNativeFunc(zipWithList1(funcIr)));
}

/// Helper function for first list argument
/// Mirrors Haskell Glue.Lib.List.ZipWith.zipWithList1 exactly
Eval<Ir> Function(Ir) zipWithList1(Ir funcIr) {
  return (Ir list1Ir) {
    return Eval.pure(IrNativeFunc(zipWithLists(funcIr, list1Ir)));
  };
}

/// Helper function for second list argument and final evaluation
/// Mirrors Haskell Glue.Lib.List.ZipWith.zipWithLists exactly
Eval<Ir> Function(Ir) zipWithLists(Ir funcIr, Ir list1Ir) {
  return (Ir list2Ir) {
    return switch ((list1Ir, list2Ir)) {
      (IrList(elements: final e1), IrList(elements: final e2)) =>
        zipWithElements(funcIr, e1.toList(), e2.toList()),
      _ => throwError(wrongArgumentType(['function', 'list', 'list'])),
    };
  };
}

/// Recursive helper function to zip two lists and apply the function element-wise
Eval<Ir> zipWithElements(Ir funcIr, List<Ir> list1, List<Ir> list2) {
  if (list1.isEmpty || list2.isEmpty) {
    return Eval.pure(IrList([]));
  }

  // Применяем функцию к первому парі элементов
  return apply(funcIr, [list1[0], list2[0]]).bind((res) {
    // Рекурсивно обрабатываем хвост
    return zipWithElements(funcIr, list1.sublist(1), list2.sublist(1)).map((
      restList,
    ) {
      final restElements = (restList as IrList).elements;
      return IrList([res, ...restElements]);
    });
  });
}
