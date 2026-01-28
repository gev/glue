import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/eval.dart';
import 'package:glue/src/either.dart';
import 'package:glue/src/runtime.dart';
import 'package:glue/src/eval/error.dart';
import 'package:glue_flutter/src/utils/color_parser.dart';

/// Utility functions for extracting values from Glue IR
/// All extraction functions use pattern matching for clean, type-safe code

/// Extract string from Glue IR value
String? extractString(Ir? value) => switch (value) {
  IrString(:final value) => value,
  IrInteger(:final value) => value.toString(),
  IrFloat(:final value) => value.toString(),
  _ => null,
};

/// Extract bool from Glue IR value
bool? extractBool(Ir? value) => switch (value) {
  IrBool(:final value) => value,
  _ => null,
};

/// Extract int from Glue IR value
int? extractInt(Ir? value) => switch (value) {
  IrInteger(:final value) => value,
  _ => null,
};

/// Extract double from Glue IR value
double? extractDouble(Ir? value) => switch (value) {
  IrInteger(:final value) => value.toDouble(),
  IrFloat(:final value) => value,
  _ => null,
};

T? extractNativeValue<T>(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: T v)) => v,
  _ => null,
};

/// Extract color from Glue IR value
Color? extractColor(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Color color)) => color,
  IrString() => parseColor(value),
  _ => null,
};

/// Extract FontWeight from Glue IR value
FontWeight? extractFontWeight(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: FontWeight weight)) => weight,
  _ => null,
};

/// Extract TextAlign from Glue IR value
TextAlign? extractTextAlign(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextAlign align)) => align,
  _ => null,
};

/// Extract MainAxisAlignment from Glue IR value
MainAxisAlignment? extractMainAxisAlignment(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: MainAxisAlignment alignment)) => alignment,
  _ => null,
};

/// Extract CrossAxisAlignment from Glue IR value
CrossAxisAlignment? extractCrossAxisAlignment(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: CrossAxisAlignment alignment)) => alignment,
  _ => null,
};

/// Extract child list from Glue IR value
Widget? extractChild(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Widget widget)) => widget,
  _ => null,
};

/// Extract children list from Glue IR value
List<Widget>? extractChildren(Ir? value) => switch (value) {
  IrList(:final elements) =>
    elements
        .map(
          (child) => switch (child) {
            IrNativeValue(value: Value(value: Widget widget)) => widget,
            _ => null,
          },
        )
        .whereType<Widget>()
        .toList(),
  _ => null,
};

/// Extract Axis from Glue IR value
Axis? extractAxis(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Axis axis)) => axis,
  _ => null,
};

/// Extract VoidCallback from Glue IR value with provided runtime
VoidCallback? extractVoidCallback(Ir? value, Runtime runtime) =>
    switch (value) {
      IrClosure(:final params) =>
        params.isEmpty
            ? () async {
                final evalAction = apply(value, []);
                // Use provided runtime instead of creating from env
                final result = await runEval(evalAction, runtime);
                switch (result) {
                  case Either<EvalError, (Ir, Runtime)> r:
                    r.match(
                      (error) => print('Callback execution error: $error'),
                      (_) {}, // Success, do nothing
                    );
                }
              }
            : null, // Only support parameterless closures for VoidCallback
      _ => null,
    };

/// Extract EdgeInsetsGeometry from Glue IR value
EdgeInsetsGeometry? extractEdgeInsets(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: EdgeInsetsGeometry insets)) => insets,
  _ => null,
};

/// Extract MainAxisSize from Glue IR value
MainAxisSize? extractMainAxisSize(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: MainAxisSize size)) => size,
  _ => null,
};

/// Extract TextDirection from Glue IR value
TextDirection? extractTextDirection(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextDirection direction)) => direction,
  _ => null,
};

/// Extract VerticalDirection from Glue IR value
VerticalDirection? extractVerticalDirection(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: VerticalDirection direction)) => direction,
  _ => null,
};

/// Extract TextBaseline from Glue IR value
TextBaseline? extractTextBaseline(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextBaseline baseline)) => baseline,
  _ => null,
};

/// Extract Clip from Glue IR value
Clip? extractClip(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Clip clip)) => clip,
  _ => null,
};

/// Extract FlutterLogoStyle from Glue IR value
FlutterLogoStyle? extractFlutterLogoStyle(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: FlutterLogoStyle style)) => style,
  _ => null,
};

/// Extract BoxFit from Glue IR value
BoxFit? extractBoxFit(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: BoxFit fit)) => fit,
  _ => null,
};

/// Extract ImageRepeat from Glue IR value
ImageRepeat? extractImageRepeat(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: ImageRepeat repeat)) => repeat,
  _ => null,
};

// /// Extract StrokeAlign from Glue IR value
// StrokeAlign? extractStrokeAlign(Ir? value) => switch (value) {
//   IrNativeValue(value: Value(value: StrokeAlign align)) => align,
//   _ => null,
// };

/// Extract TextOverflow from Glue IR value
TextOverflow? extractTextOverflow(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextOverflow overflow)) => overflow,
  _ => null,
};

/// Extract TextWidthBasis from Glue IR value
TextWidthBasis? extractTextWidthBasis(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextWidthBasis basis)) => basis,
  _ => null,
};

/// Extract DragStartBehavior from Glue IR value
DragStartBehavior? extractDragStartBehavior(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: DragStartBehavior behavior)) => behavior,
  _ => null,
};

/// Extract FilterQuality from Glue IR value
FilterQuality? extractFilterQuality(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: FilterQuality quality)) => quality,
  _ => null,
};

/// Extract FloatingActionButtonLocation from Glue IR value
FloatingActionButtonLocation? extractFloatingActionButtonLocation(Ir? value) =>
    switch (value) {
      IrNativeValue(
        value: Value(value: FloatingActionButtonLocation location),
      ) =>
        location,
      _ => null,
    };

/// Extract TextCapitalization from Glue IR value
TextCapitalization? extractTextCapitalization(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextCapitalization capitalization)) =>
    capitalization,
  _ => null,
};

/// Extract TextInputType from Glue IR value
TextInputType? extractTextInputType(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TextInputType inputType)) => inputType,
  _ => null,
};

/// Extract Brightness from Glue IR value
Brightness? extractBrightness(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Brightness brightness)) => brightness,
  _ => null,
};

/// Extract Duration from Glue IR value
Duration? extractDuration(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Duration duration)) => duration,
  _ => null,
};

/// Extract Curve from Glue IR value
Curve? extractCurve(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: Curve curve)) => curve,
  _ => null,
};

/// Extract DateTime from Glue IR value
DateTime? extractDateTime(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: DateTime dateTime)) => dateTime,
  _ => null,
};

/// Extract TimeOfDay from Glue IR value
TimeOfDay? extractTimeOfDay(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: TimeOfDay timeOfDay)) => timeOfDay,
  _ => null,
};

/// Extract BoxShape from Glue IR value
BoxShape? extractBoxShape(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: BoxShape boxShape)) => boxShape,
  _ => null,
};

/// Extract VisualDensity from Glue IR value
VisualDensity? extractVisualDensity(Ir? value) => switch (value) {
  IrNativeValue(value: Value(value: VisualDensity visualDensity)) =>
    visualDensity,
  _ => null,
};
