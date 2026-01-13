import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'builtin/def.dart';
import 'builtin/lambda.dart';

/// Builtin module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The builtin module containing all special forms
/// TODO: Implement special forms properly
final ModuleInfo builtin = nativeModule('ffi.builtin', [
  // TODO: Add special forms when they're properly implemented
  // ('def', IrNative(Native(NativeSpecial(def)))),
  // ('lambda', IrNative(Native(NativeSpecial(lambda)))),
  // ('\\', IrNative(Native(NativeSpecial(lambda)))),
]);
