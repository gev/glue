import 'package:glue/lib/bool.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'reactive_module.dart';

final env = envFromModules([
  builtinModule,
  boolModule,
  uiModule,
  reactiveModule,
]);
