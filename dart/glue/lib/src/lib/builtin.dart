import 'package:glue/src/lib/builtin/def.dart';
import 'package:glue/src/lib/builtin/error.dart';
import 'package:glue/src/lib/builtin/import.dart';
import 'package:glue/src/lib/builtin/lambda.dart';
import 'package:glue/src/lib/builtin/let.dart';
import 'package:glue/src/lib/builtin/quote.dart';
import 'package:glue/src/lib/builtin/try.dart';
import 'package:glue/src/module.dart';

/// Builtin module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The builtin module containing all special forms
/// Mirrors Haskell Glue.Lib.Builtin.builtin exactly
final ModuleInfo builtinModule = nativeModule('ffi.builtin', [
  ('def', def),
  ('lambda', lambda),
  ('\\', lambda),
  ('let', let),
  ('import', importForm),
  ('error', error),
  ('try', tryFunc),
  ('quote', quote),
]);
