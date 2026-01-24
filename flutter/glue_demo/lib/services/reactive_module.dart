import 'package:glue/module.dart';
import 'reactive_functions.dart';

/// Reactive module providing reactive state management and UI components
final reactiveModule = nativeModule('reactive', [
  ('reactive-counter', reactiveCounter),
  ('reactive-widget', reactiveWidget),
]);
