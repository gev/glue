import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'bool/eq.dart';
import 'bool/ge.dart';
import 'bool/gt.dart';
import 'bool/if.dart';
import 'bool/le.dart';
import 'bool/lt.dart';
import 'bool/ne.dart';
import 'bool/not.dart';
import 'bool/until.dart';
import 'bool/when.dart';
import 'bool/while.dart';

/// Bool module - boolean operations, comparisons, and control flow
/// Mirrors Haskell Glue.Lib.Bool exactly

/// The bool module containing all boolean functions and special forms
/// Mirrors Haskell Glue.Lib.Bool.bool exactly
final ModuleInfo bool = nativeModule('ffi.bool', [
  // Constants
  ('true', IrBool(true)),
  ('false', IrBool(false)),

  // Comparison functions
  ('eq', IrNative(NativeFunc(eq))),
  ('==', IrNative(NativeFunc(eq))),
  ('ne', IrNative(NativeFunc(ne))),
  ('!=', IrNative(NativeFunc(ne))),
  ('lt', IrNative(NativeFunc(lt))),
  ('<', IrNative(NativeFunc(lt))),
  ('le', IrNative(NativeFunc(le))),
  ('<=', IrNative(NativeFunc(le))),
  ('gt', IrNative(NativeFunc(gt))),
  ('>', IrNative(NativeFunc(gt))),
  ('ge', IrNative(NativeFunc(ge))),
  ('>=', IrNative(NativeFunc(ge))),

  // Logical functions
  ('not', IrNative(NativeFunc(not))),
  ('!', IrNative(NativeFunc(not))),

  // Special forms
  ('if', IrNative(NativeSpecial(if_))),
  ('when', IrNative(NativeSpecial(when_))),
  ('while', IrNative(NativeSpecial(while_))),
  ('until', IrNative(NativeSpecial(until_))),
]);
