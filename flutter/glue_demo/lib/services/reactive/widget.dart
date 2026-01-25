import 'package:glue/module.dart';
import 'package:glue_demo/services/reactive/widget/widget_function.dart';

/// Reactive module providing reactive state management and UI components
final widgetModule = nativeModule('reactive', [
  ('reactive-widget', reactiveWidget),
]);
