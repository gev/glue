import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/eval/exception.dart';
import 'widgets/glue_text.dart';
import 'widgets/glue_button.dart';
import 'widgets/glue_container.dart';
import 'widgets/glue_column.dart';
import 'widgets/glue_row.dart';
import 'widgets/glue_padding.dart';
import 'widgets/glue_center.dart';

/// UI module - Flutter implementation of framework-agnostic UI API
/// Provides concrete Flutter rendering for abstract UI specifications

/// The ui module containing all UI functions
/// Implements the framework-agnostic UI API with Flutter widgets
final ModuleInfo ui = nativeModule('ui', [
  // Core widget functions
  ('text', text),
  ('button', button),
  ('container', container),
  ('column', column),
  ('row', row),
  ('padding', padding),
  ('center', center),
]);

/// ============================================================================
/// CORE WIDGET FUNCTIONS
/// ============================================================================

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

/// Button widget function
/// Creates Flutter ElevatedButton from Glue (button props) expressions
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object (label is in props)
Eval<Ir> buttonImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final buttonWidget = GlueButton(props.properties.unlock);
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}

/// Container widget function
/// Creates Flutter Column/Row from Glue (container props) expressions
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object (children in props)
Eval<Ir> containerImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final containerWidget = GlueContainer(props.properties.unlock);
  return Eval.pure(IrNativeValue(HostValue(containerWidget)));
}

/// Column widget function
/// Creates Flutter Column from Glue (column props) expressions
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final columnWidget = GlueColumn(props.properties.unlock);
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}

/// Row widget function
/// Creates Flutter Row from Glue (row props) expressions
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final rowWidget = GlueRow(props.properties.unlock);
  return Eval.pure(IrNativeValue(HostValue(rowWidget)));
}

/// Padding widget function
/// Creates Flutter Padding from Glue (padding child props) expressions
final Ir padding = IrNativeFunc(paddingImpl);

/// Padding implementation - takes child, then properties
Eval<Ir> paddingImpl(Ir child) {
  return Eval.pure(IrNativeFunc(paddingWithChild(child)));
}

/// Padding with child - takes properties object
Eval<Ir> Function(Ir) paddingWithChild(Ir child) {
  return (Ir props) {
    if (child is! IrNativeValue) {
      return throwError(wrongArgumentType(['widget']));
    }
    if (props is! IrObject) {
      return throwError(wrongArgumentType(['object']));
    }

    final paddingWidget = GluePadding(child, props.properties.unlock);
    return Eval.pure(IrNativeValue(HostValue(paddingWidget)));
  };
}

/// Center widget function
/// Creates Flutter Center from Glue (center child) expressions
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes child
Eval<Ir> centerImpl(Ir child) {
  if (child is! IrNativeValue) {
    return throwError(wrongArgumentType(['widget']));
  }

  final centerWidget = GlueCenter(child);
  return Eval.pure(IrNativeValue(HostValue(centerWidget)));
}
