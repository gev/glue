import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/ir.dart' as ir;

final Ir exists = IrNativeFunc(existsImpl);

Eval<Ir> existsImpl(Ir arg) => Eval.pure(IrBool(ir.exists(arg)));
