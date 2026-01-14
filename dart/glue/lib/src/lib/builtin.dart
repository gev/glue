import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'builtin/def.dart';
import 'builtin/lambda.dart';

/// Builtin module - special forms and core language constructs
/// Mirrors Haskell Glue.Lib.Builtin exactly

/// The builtin module containing all special forms
/// Mirrors Haskell Glue.Lib.Builtin.builtin exactly
final ModuleInfo builtin = nativeModule('ffi.builtin', [
  ('def', IrNative(NativeSpecial(def))),
  ('set', IrNative(NativeSpecial(_set))), // TODO: implement set
  ('lambda', IrNative(NativeSpecial(lambda))),
  ('\\', IrNative(NativeSpecial(lambda))), // backslash is lambda
  ('let', IrNative(NativeSpecial(_let))), // TODO: implement let
  ('import', IrNative(NativeSpecial(_import))), // TODO: implement import
  ('error', IrNative(NativeSpecial(_error))), // TODO: implement error
  ('try', IrNative(NativeSpecial(_try))), // TODO: implement try
]);

/// Placeholder implementations for unimplemented special forms
/// These will throw "not implemented" errors until properly implemented

Eval<Ir> _set(List<Ir> args) =>
    throw UnimplementedError('set special form not implemented');

Eval<Ir> _let(List<Ir> args) =>
    throw UnimplementedError('let special form not implemented');

Eval<Ir> _import(List<Ir> args) =>
    throw UnimplementedError('import special form not implemented');

Eval<Ir> _error(List<Ir> args) =>
    throw UnimplementedError('error special form not implemented');

Eval<Ir> _try(List<Ir> args) =>
    throw UnimplementedError('try special form not implemented');

// Export the implemented special forms for use in eval.dart
