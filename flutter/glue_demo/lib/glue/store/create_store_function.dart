import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_demo/glue/store/store.dart';

/// Creates a new store instance read
final createStoreFunction = IrEvaluable(
  () => Eval.pure(IrNativeValue(Value(Store()))),
);
