import 'package:glue/module.dart';
import 'package:glue_demo/services/state/listen_function.dart';
import 'package:glue_demo/services/state/modify_function.dart';
import 'package:glue_demo/services/state/read_function.dart';
import 'package:glue_demo/services/state/state_function.dart';
import 'package:glue_demo/services/state/write_function.dart';

/// State module providing reactive state management and UI components
final stateModule = nativeModule('state', [
  ('state', stateFunction),
  ('listen', listenFunction),
  ('read', readFunction),
  ('write', writeFunction),
  ('modify', modifyFunction),
]);
