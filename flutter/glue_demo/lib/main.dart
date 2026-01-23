import 'package:flutter/material.dart';
import 'package:glue_demo/widgets/glue_demo.dart';

void main() {
  runApp(const GlueDemoApp());
}

class GlueDemoApp extends StatelessWidget {
  const GlueDemoApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Glue Demo  Live UI Editor',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: Colors.blue),
      ),
      darkTheme: ThemeData(
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.blue,
          brightness: Brightness.dark,
        ),
      ),
      themeMode: ThemeMode.system,
      home: const GlueDemo(),
    );
  }
}
