import 'dart:io';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

/// Read line function - reads a line from stdin
/// Mirrors Haskell Glue.Lib.IO.Read.readLine exactly
final Ir readLine = IrNativeFunc(readLineImpl);

/// Read line function implementation
/// Mirrors Haskell Glue.Lib.IO.Read.readLineImpl exactly
Eval<Ir> readLineImpl(Ir arg) {
  // Ignore the argument, always read a line
  return liftIO(stdin.readLineSync() ?? '').map((line) => IrString(line));
}
