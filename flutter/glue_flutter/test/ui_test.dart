import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:glue/src/ir.dart';
import 'package:glue/src/module.dart';
import 'package:glue_flutter/src/ui.dart';
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
      expect(text, isA<IrNativeFunc>());
    });

    test('button function returns IrNativeFunc', () {
      expect(button, isA<IrNativeFunc>());
    });

    test('container function returns IrNativeFunc', () {
      expect(container, isA<IrNativeFunc>());
    });

    test('column function returns IrNativeFunc', () {
      expect(column, isA<IrNativeFunc>());
    });

    test('row function returns IrNativeFunc', () {
      expect(row, isA<IrNativeFunc>());
    });

    test('padding function returns IrNativeFunc', () {
      expect(padding, isA<IrNativeFunc>());
    });

    test('center function returns IrNativeFunc', () {
      expect(center, isA<IrNativeFunc>());
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
