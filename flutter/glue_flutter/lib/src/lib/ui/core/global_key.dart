import 'package:flutter/cupertino.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';

final globalKey = IrEvaluable(
  () => Eval.pure(IrNativeValue(Value(GlobalKey()))),
);
