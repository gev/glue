import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'io/print.dart';
import 'io/read.dart';

/// IO module - input/output functions
/// Mirrors Haskell Glue.Lib.IO exactly

/// The io module containing all IO functions
/// Mirrors Haskell Glue.Lib.IO.io exactly
final ModuleInfo io = nativeModule('ffi.io', [
  ('print', IrNative(NativeFunc(printFunc))),
  ('println', IrNative(NativeFunc(println))),
  ('read-line', IrNative(NativeFunc(readLine))),
]);
