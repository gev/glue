import 'package:glue/module.dart';
import 'package:glue_demo/services/state/listen_function.dart';
import 'package:glue_demo/services/state/set_function.dart';
import 'package:glue_demo/services/state/state_function.dart';

/// State module providing reactive state management and UI components
final stateModule = nativeModule('state', [
  ('listen', listenFunction),
  ('set', setFunction),
  ('state', stateFunction),
]);
