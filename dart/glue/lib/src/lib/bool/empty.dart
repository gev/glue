import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/ir.dart' as ir;

final Ir empty = IrNativeFunc(emptyImpl);

Eval<Ir> emptyImpl(Ir arg) => Eval.pure(IrBool(ir.empty(arg)));
