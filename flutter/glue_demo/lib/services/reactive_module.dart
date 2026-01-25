import 'package:glue/module.dart';
import 'package:glue_demo/services/reactive_counter_function.dart';
import 'package:glue_demo/services/reactive_widget_function.dart';

/// Reactive module providing reactive state management and UI components
final reactiveModule = nativeModule('reactive', [
  ('reactive-counter', reactiveCounter),
  ('inc', inc),
  ('dec', dec),
  ('reactive-widget', reactiveWidget),
]);
