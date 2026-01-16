import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'builtin/def.dart';
import 'builtin/error.dart';
import 'builtin/import.dart';
import 'builtin/lambda.dart';
import 'builtin/let.dart';
import 'builtin/set.dart';
import 'builtin/try.dart';

/// Builtin module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The builtin module containing all special forms
/// Mirrors Haskell Glue.Lib.Builtin.builtin exactly
final ModuleInfo builtin = nativeModule('ffi.builtin', [
  ('def', IrSpecial(def)),
  ('set', IrSpecial(set)),
  ('lambda', IrSpecial(lambda)),
  ('\\', IrSpecial(lambda)), // backslash is lambda
  ('let', IrSpecial(let)),
  ('import', IrSpecial(importForm)),
  ('error', IrSpecial(error)),
  ('try', IrSpecial(tryFunc)),
]);

// Export the implemented special forms for use in eval.dart
