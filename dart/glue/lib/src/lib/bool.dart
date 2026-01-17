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
  ('eq', IrNativeFunc(eq)),
  ('==', IrNativeFunc(eq)),
  ('ne', IrNativeFunc(ne)),
  ('!=', IrNativeFunc(ne)),
  ('lt', IrNativeFunc(lt)),
  ('<', IrNativeFunc(lt)),
  ('le', IrNativeFunc(le)),
  ('<=', IrNativeFunc(le)),
  ('gt', IrNativeFunc(gt)),
  ('>', IrNativeFunc(gt)),
  ('ge', IrNativeFunc(ge)),
  ('>=', IrNativeFunc(ge)),

  // Logical functions
  ('not', IrNativeFunc(not)),
  ('!', IrNativeFunc(not)),

  // Special forms
  ('if', IrSpecial(if_)),
  ('when', IrSpecial(when_)),
  ('while', IrSpecial(while_)),
  ('until', IrSpecial(until_)),
]);
