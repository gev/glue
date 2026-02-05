import 'package:glue/module.dart';
import 'package:glue_flutter/src/lib/store/create_store_function.dart';
import 'package:glue_flutter/src/lib/store/get_function.dart';
import 'package:glue_flutter/src/lib/store/put_function.dart';

/// Generic store module providing key-value storage
final storeModule = nativeModule('ffi.store', [
  ('create-store', createStoreFunction),
  ('put', put),
  ('get', get),
]);
