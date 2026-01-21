import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval/exception.dart';
import '../../widgets/glue_text.dart';

/// Text widget function
/// Creates Flutter Text widget from Glue (text content props) expressions
final Ir text = IrNativeFunc(textImpl);

/// Text implementation - takes content string
Eval<Ir> textImpl(Ir content) {
  return Eval.pure(IrNativeFunc(textWithContent(content)));
}

/// Text with content - takes properties object
Eval<Ir> Function(Ir) textWithContent(Ir content) {
  return (Ir props) {
    if (content is! IrString) {
      return throwError(wrongArgumentType(['string']));
    }
    if (props is! IrObject) {
      return throwError(wrongArgumentType(['object']));
    }

    final textWidget = GlueText(content.value, props.properties.unlock);
    return Eval.pure(IrNativeValue(HostValue(textWidget)));
  };
}
