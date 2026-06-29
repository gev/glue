import 'package:glue/src/ir.dart';
import 'package:glue/src/lib/bool/and.dart';
import 'package:glue/src/lib/bool/eq.dart';
import 'package:glue/src/lib/bool/fallback.dart';
import 'package:glue/src/lib/bool/ge.dart';
import 'package:glue/src/lib/bool/gt.dart';
import 'package:glue/src/lib/bool/if.dart';
import 'package:glue/src/lib/bool/is_empty.dart';
import 'package:glue/src/lib/bool/is_exist.dart';
import 'package:glue/src/lib/bool/le.dart';
import 'package:glue/src/lib/bool/lt.dart';
import 'package:glue/src/lib/bool/maybe.dart';
import 'package:glue/src/lib/bool/ne.dart';
import 'package:glue/src/lib/bool/not.dart';
import 'package:glue/src/lib/bool/or.dart';
import 'package:glue/src/module.dart';

/// Bool module - boolean operations, comparisons, and control flow
/// Mirrors Haskell Glue.Lib.Bool exactly

/// The bool module containing all boolean functions and special forms
/// Mirrors Haskell Glue.Lib.Bool.bool exactly
final ModuleInfo boolModule = nativeModule('ffi.bool', [
  ('true', IrBool(true)),
  ('false', IrBool(false)),
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

  ('not', not),
  ('!', not),
  ('and', and_),
  ('&&', and_),
  ('or', or_),
  ('||', or_),

  ('is-empty', isEmpty_),
  ('is-exist', isExist_),

  ('fallback', fallback),
  ('??', fallback),

  ('maybe', maybe),
  ('?>', maybe),

  ('if', if_),
  ('?', if_),
]);
