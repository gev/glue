import 'package:glue/module.dart';
import 'package:glue_flutter/src/lib/ui/state/listen_function.dart';
import 'package:glue_flutter/src/lib/ui/state/modify_function.dart';
import 'package:glue_flutter/src/lib/ui/state/read_function.dart';
import 'package:glue_flutter/src/lib/ui/state/state_function.dart';
import 'package:glue_flutter/src/lib/ui/state/write_function.dart';

/// State module providing reactive state management and UI components
final stateModule = nativeModule('ffi.ui.state', [
  ('state', stateFunction),
  ('listen', listenFunction),
  ('read', readFunction),
  ('write', writeFunction),
  ('modify', modifyFunction),
]);
