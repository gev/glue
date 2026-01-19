import 'dart:io';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Read line function - reads a line from stdin
/// Mirrors Haskell Glue.Lib.IO.Read.readLine exactly
final Ir readLine = IrNativeFunc(readLineImpl);

/// Read line function implementation
/// Mirrors Haskell Glue.Lib.IO.Read.readLineImpl exactly
Eval<Ir> readLineImpl(List<Ir> args) {
  return switch (args) {
    [] => liftIO(stdin.readLineSync() ?? '').map((line) => IrString(line)),
    _ => Eval.pure(
      IrString(''),
    ), // Haskell version returns empty string for wrong arguments
  };
}
