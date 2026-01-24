import 'package:glue/lib/bool.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'reactive.dart';

final env = envFromModules([
  builtinModule,
  boolModule,
  uiModule,
  nativeModule('reactive', [
    ('reactive-counter', reactiveCounter),
    ('reactive-widget', reactiveWidget),
  ]),
]);
