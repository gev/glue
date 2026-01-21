import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue_flutter/src/lib/ui.dart';
import 'package:glue_flutter/src/widgets/glue_text.dart';

void main() {
  group('UI Module', () {
    test('module is properly defined', () {
      expect(ui, isA<ModuleInfo>());
      expect(ui.moduleName, 'ui');
      expect(ui.exports, contains('text'));
      expect(ui.exports, contains('button'));
      expect(ui.exports, contains('container'));
      expect(ui.exports, contains('column'));
      expect(ui.exports, contains('row'));
      expect(ui.exports, contains('padding'));
      expect(ui.exports, contains('center'));
    });

    test('text function returns IrNativeFunc', () {
      final textDef = ui.definitions.firstWhere((def) => def.$1 == 'text');
      expect(textDef.$2, isA<IrNativeFunc>());
    });

    test('button function returns IrNativeFunc', () {
      final buttonDef = ui.definitions.firstWhere((def) => def.$1 == 'button');
      expect(buttonDef.$2, isA<IrNativeFunc>());
    });

    test('container function returns IrNativeFunc', () {
      final containerDef = ui.definitions.firstWhere(
        (def) => def.$1 == 'container',
      );
      expect(containerDef.$2, isA<IrNativeFunc>());
    });

    test('column function returns IrNativeFunc', () {
      final columnDef = ui.definitions.firstWhere((def) => def.$1 == 'column');
      expect(columnDef.$2, isA<IrNativeFunc>());
    });

    test('row function returns IrNativeFunc', () {
      final rowDef = ui.definitions.firstWhere((def) => def.$1 == 'row');
      expect(rowDef.$2, isA<IrNativeFunc>());
    });

    test('padding function returns IrNativeFunc', () {
      final paddingDef = ui.definitions.firstWhere(
        (def) => def.$1 == 'padding',
      );
      expect(paddingDef.$2, isA<IrNativeFunc>());
    });

    test('center function returns IrNativeFunc', () {
      final centerDef = ui.definitions.firstWhere((def) => def.$1 == 'center');
      expect(centerDef.$2, isA<IrNativeFunc>());
    });
  });

  group('Widget Creation', () {
    testWidgets('GlueText builds correctly', (WidgetTester tester) async {
      final properties = <String, dynamic>{};
      final glueText = GlueText('Hello', properties);

      await tester.pumpWidget(MaterialApp(home: glueText));

      expect(find.text('Hello'), findsOneWidget);
    });

    testWidgets('GlueText with properties builds correctly', (
      WidgetTester tester,
    ) async {
      final properties = <String, dynamic>{
        'color': IrString('blue'),
        'size': IrInteger(24),
        'weight': IrString('bold'),
      };
      final glueText = GlueText('Styled', properties);

      await tester.pumpWidget(MaterialApp(home: glueText));

      expect(find.text('Styled'), findsOneWidget);
      final textWidget = tester.widget<Text>(find.text('Styled'));
      expect(textWidget.style?.color, Colors.blue);
      expect(textWidget.style?.fontSize, 24.0);
      expect(textWidget.style?.fontWeight, FontWeight.bold);
    });
  });
}
