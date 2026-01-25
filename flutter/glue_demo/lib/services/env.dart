import 'package:glue/lib/bool.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/lib/math/arithmetic.dart';
import 'package:glue/module.dart';
import 'package:glue_demo/services/reactive/counter.dart';
import 'package:glue_demo/services/reactive/widget.dart';
import 'package:glue_flutter/glue_flutter.dart';

final env = envFromModules([
  builtinModule,
  boolModule,
  arithmeticModule,
  uiModule,
  counterModule,
  widgetModule,
]);
