import 'package:glue/src/module.dart';
import 'package:glue/src/lib/builtin/def.dart';
import 'package:glue/src/lib/builtin/error.dart';
import 'package:glue/src/lib/builtin/import.dart';
import 'package:glue/src/lib/builtin/lambda.dart';
import 'package:glue/src/lib/builtin/let.dart';
import 'package:glue/src/lib/builtin/set.dart';
import 'package:glue/src/lib/builtin/try.dart';

/// Builtin module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The builtin module containing all special forms
/// Mirrors Haskell Glue.Lib.Builtin.builtin exactly
final ModuleInfo builtinModule = nativeModule('ffi.builtin', [
  ('def', def),
  ('set', set),
  ('lambda', lambda),
  ('\\', lambda), // backslash is lambda
  ('let', let),
  ('import', importForm),
  ('error', error),
  ('try', tryFunc),
]);

// Export the implemented special forms for use in eval.dart
