import 'package:glue/src/lib/module/import.dart';
import 'package:glue/src/module.dart';

/// Module module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The Module module containing all special forms
/// Mirrors Haskell Glue.Lib.Module exactly
final ModuleInfo moduleModule = nativeModule('ffi.module', [
  ('import', importForm),
]);
