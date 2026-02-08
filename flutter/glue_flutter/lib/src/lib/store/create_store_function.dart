import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/lib/store/store.dart';

/// Creates a new store instance read
final createStoreFunction = IrNativeFunc(
  (_) => Eval.pure(IrNativeValue(Value(Store()))),
);
