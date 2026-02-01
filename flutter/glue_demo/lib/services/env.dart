import 'package:glue/lib/bool.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/lib/list.dart';
import 'package:glue/lib/math/arithmetic.dart';
import 'package:glue/module.dart';
import 'package:glue_demo/services/state/state_nodule.dart';
import 'package:glue_flutter/glue_flutter.dart';

final env = envFromModules([
  builtinModule,
  boolModule,
  arithmeticModule,
  listModule,
  uiCoreModule,
  uiMaterialModule,
  uiCupertinoModule,
  stateModule,
]);
