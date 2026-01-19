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
  ('==', eq),
  ('ne', ne),
  ('!=', ne),
  ('lt', lt),
  ('<', lt),
  ('le', le),
  ('<=', le),
  ('gt', gt),
  ('>', gt),
  ('ge', ge),
  ('>=', ge),

  // Logical functions
  ('not', not),
  ('!', not),

  // Special forms
  ('if', if_),
  ('when', when_),
  ('while', while_),
  ('until', until_),
]);
