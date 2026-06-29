import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

final Ir isEmpty_ = IrNativeFunc(isEmptyImpl);

Eval<Ir> isEmptyImpl(Ir arg) => Eval.pure(IrBool(isEmpty(arg)));
