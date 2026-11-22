import 'package:glue/lib/bool.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/lib/list.dart';
import 'package:glue/lib/math/arithmetic.dart';
import 'package:glue/module.dart';
import 'package:glue_demo/glue/state.dart';
import 'package:glue_demo/glue/store.dart';
import 'package:glue_flutter_cupertino_ui/cupertino_ui.dart';
import 'package:glue_flutter_material_ui/material_ui.dart';
import 'package:glue_flutter_ui/ui.dart';

final env = envFromModules([
  builtinModule,
  boolModule,
  arithmeticModule,
  listModule,
  uiCoreModule,
  uiMaterialModule,
  uiCupertinoModule,
  stateModule,
  storeModule,
]);
