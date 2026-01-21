import 'package:flutter/material.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue/src/eval/exception.dart';
import 'widgets/glue_widget.dart';
import 'widgets/glue_text.dart';
import 'widgets/glue_button.dart';
import 'widgets/glue_container.dart';
import 'widgets/glue_column.dart';
import 'widgets/glue_row.dart';
import 'widgets/glue_padding.dart';
import 'widgets/glue_center.dart';
import 'utils/color_parser.dart';
import 'utils/font_weight_parser.dart';
import 'utils/text_align_parser.dart';
import 'utils/main_axis_alignment_parser.dart';
import 'utils/cross_axis_alignment_parser.dart';
import 'utils/edge_insets_parser.dart';

/// UI module - Flutter implementation of framework-agnostic UI API
/// Mirrors Haskell Glue.UI exactly

/// The ui module containing all UI functions
/// Mirrors Haskell Glue.UI.ui exactly
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
/// Mirrors Haskell Glue.UI.Text.text exactly
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

    final textWidget = GlueText(content.value, props.properties);
    return Eval.pure(IrNativeValue(HostValue(textWidget)));
  };
}

/// Button widget function
/// Mirrors Haskell Glue.UI.Button.button exactly
final Ir button = IrNativeFunc(buttonImpl);

/// Button implementation - takes properties object (label is in props)
Eval<Ir> buttonImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final buttonWidget = GlueButton(props.properties);
  return Eval.pure(IrNativeValue(HostValue(buttonWidget)));
}

/// Container widget function
/// Mirrors Haskell Glue.UI.Container.container exactly
final Ir container = IrNativeFunc(containerImpl);

/// Container implementation - takes properties object (children in props)
Eval<Ir> containerImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final containerWidget = GlueContainer(props.properties);
  return Eval.pure(IrNativeValue(HostValue(containerWidget)));
}

/// Column widget function
/// Mirrors Haskell Glue.UI.Column.column exactly
final Ir column = IrNativeFunc(columnImpl);

/// Column implementation - takes properties object
Eval<Ir> columnImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final columnWidget = GlueColumn(props.properties);
  return Eval.pure(IrNativeValue(HostValue(columnWidget)));
}

/// Row widget function
/// Mirrors Haskell Glue.UI.Row.row exactly
final Ir row = IrNativeFunc(rowImpl);

/// Row implementation - takes properties object
Eval<Ir> rowImpl(Ir props) {
  if (props is! IrObject) {
    return throwError(wrongArgumentType(['object']));
  }

  final rowWidget = GlueRow(props.properties);
  return Eval.pure(IrNativeValue(HostValue(rowWidget)));
}

/// Padding widget function
/// Mirrors Haskell Glue.UI.Padding.padding exactly
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

    final paddingWidget = GluePadding(child, props.properties);
    return Eval.pure(IrNativeValue(HostValue(paddingWidget)));
  };
}

/// Center widget function
/// Mirrors Haskell Glue.UI.Center.center exactly
final Ir center = IrNativeFunc(centerImpl);

/// Center implementation - takes child
Eval<Ir> centerImpl(Ir child) {
  if (child is! IrNativeValue) {
    return throwError(wrongArgumentType(['widget']));
  }

  final centerWidget = GlueCenter(child);
  return Eval.pure(IrNativeValue(HostValue(centerWidget)));
}
