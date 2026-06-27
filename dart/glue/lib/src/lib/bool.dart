import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/bool/eq.dart';
import 'package:glue/src/lib/bool/ge.dart';
import 'package:glue/src/lib/bool/gt.dart';
import 'package:glue/src/lib/bool/if.dart';
import 'package:glue/src/lib/bool/le.dart';
import 'package:glue/src/lib/bool/lt.dart';
import 'package:glue/src/lib/bool/ne.dart';
import 'package:glue/src/lib/bool/not.dart';
import 'package:glue/src/lib/bool/when.dart';
import 'package:glue/src/module.dart';

/// Bool module - boolean operations, comparisons, and control flow
/// Mirrors Haskell Glue.Lib.Bool exactly

/// The bool module containing all boolean functions and special forms
/// Mirrors Haskell Glue.Lib.Bool.bool exactly
final ModuleInfo boolModule = nativeModule('ffi.bool', [
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
]);
