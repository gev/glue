import 'package:glue/module.dart';
import 'package:glue_demo/glue/store/create_store_function.dart';
import 'package:glue_demo/glue/store/get_function.dart';
import 'package:glue_demo/glue/store/put_function.dart';

/// Generic store module providing key-value storage
final storeModule = nativeModule('ffi.store', [
  ('create-store', createStoreFunction),
  ('put', put),
  ('get', get),
]);
