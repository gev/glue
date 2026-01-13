import 'package:glue/module.dart';
import 'package:glue/src/lib/builtin/def.dart';
import 'package:glue/src/lib/builtin/lambda.dart';

/// Builtin module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The builtin module containing all special forms
final ModuleInfo builtin = nativeModule('ffi.builtin', [
  ('def', Native(NativeSpecial(def))),
  ('lambda', Native(NativeSpecial(lambda))),
  ('\\', Native(NativeSpecial(lambda))),
  // TODO: Add other special forms as implemented
  // ('set', Native(NativeSpecial(set))),
  // ('let', Native(NativeSpecial(let))),
  // ('import', Native(NativeSpecial(importForm))),
  // ('error', Native(NativeSpecial(errorFunc))),
  // ('try', Native(NativeSpecial(tryFunc))),
]);
