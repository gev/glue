import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

final Ir isExist_ = IrNativeFunc(isExistImpl);

Eval<Ir> isExistImpl(Ir arg) => Eval.pure(IrBool(isExist(arg)));
