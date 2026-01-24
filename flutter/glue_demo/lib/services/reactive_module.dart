import 'package:glue/module.dart';
import 'reactive_counter_function.dart';
import 'reactive_widget_function.dart';

/// Reactive module providing reactive state management and UI components
final reactiveModule = nativeModule('reactive', [
  ('reactive-counter', reactiveCounter),
  ('reactive-widget', reactiveWidget),
]);
