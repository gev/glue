import 'package:glue/module.dart';
import 'package:glue_demo/glue/state/listen_function.dart';
import 'package:glue_demo/glue/state/modify_function.dart';
import 'package:glue_demo/glue/state/read_function.dart';
import 'package:glue_demo/glue/state/state_function.dart';
import 'package:glue_demo/glue/state/write_function.dart';

/// State module providing reactive state management and UI components
final stateModule = nativeModule('ffi.ui.state', [
  ('state', stateFunction),
  ('listen', listenFunction),
  ('read', readFunction),
  ('write', writeFunction),
  ('modify', modifyFunction),
]);
