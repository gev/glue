import 'package:glue/lib/bool.dart';
import 'package:glue/lib/builtin.dart';
import 'package:glue/module.dart';
import 'package:glue_flutter/glue_flutter.dart';
import 'reactive_functions.dart';

/// Reactive module providing reactive state management and UI components
final reactiveModule = nativeModule('reactive', [
  ('reactive-counter', reactiveCounter),
  ('reactive-widget', reactiveWidget),
]);

final env = envFromModules([
  builtinModule,
  boolModule,
  uiModule,
  reactiveModule,
]);
