import 'package:glue/module/import.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/eval/exception.dart';
import 'package:glue/src/ir.dart';

/// Import special form - loads and evaluates a module
/// Mirrors Haskell Glue.Lib.Module.Import.importForm exactly
final Ir importForm = IrSpecial(importFormImpl);

/// Import special form implementation
/// Mirrors Haskell Glue.Lib.Module.Import.importForm exactly
Eval<Ir> importFormImpl(List<Ir> args) {
  if (args.length != 1) {
    return throwError(wrongArgumentType(['module-name']));
  }

  final moduleNameIr = args[0];
  return switch (moduleNameIr) {
    IrSymbol(value: final name) => importModule(name),
    IrDottedSymbol(value: final name) => importModule(name),
    _ => throwError(wrongArgumentType(['module-name'])),
  };
}
