import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter_ui/utils/widget_properties.dart';
import 'package:material_ui/material_ui.dart';

final theme = IrNativeFunc((props) => _createTheme(WidgetProperties(props)));

Eval<Ir> _createTheme(WidgetProperties properties) {
  final colorScheme = properties.getValue<ColorScheme>('color-scheme');
  final brightness = properties.getValue<Brightness>('brightness');

  final themeData = ThemeData(
    useMaterial3: true,
    colorScheme: colorScheme,
    brightness: brightness,
  );

  return Eval.pure(IrNativeValue(Value(themeData)));
}
