import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';

final Ir list = IrSpecial(
  (args) => sequenceAll(args.map(eval).toList()).map(IrList.new),
);
