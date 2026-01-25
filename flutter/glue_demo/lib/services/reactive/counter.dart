import 'package:glue/module.dart';
import 'package:glue_demo/services/reactive/counter/counter_function.dart';
import 'package:glue_demo/services/reactive/counter/dec_function.dart';
import 'package:glue_demo/services/reactive/counter/inc_function.dart';

/// Reactive module providing reactive state management and UI components
final counterModule = nativeModule('reactive', [
  ('reactive-counter', counterFunction),
  ('inc', incFunction),
  ('dec', dec),
]);
