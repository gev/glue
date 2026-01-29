import 'package:glue/src/lib/io/print.dart';
import 'package:glue/src/lib/io/read.dart';
import 'package:glue/src/module.dart';

/// IO module - input/output functions
/// Mirrors Haskell Glue.Lib.IO exactly

/// The io module containing all IO functions
/// Mirrors Haskell Glue.Lib.IO.io exactly
final ModuleInfo ioModule = nativeModule('ffi.io', [
  ('print', printFunc),
  ('println', println),
  ('read-line', readLine),
]);
