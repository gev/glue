import 'package:flutter/material.dart';

/// Base class for all Glue Flutter widgets
/// Provides common functionality and ensures all widgets extend StatelessWidget
abstract class GlueWidget extends StatelessWidget {
  /// Properties map containing widget configuration from Glue
  final Map<String, dynamic> properties;

  const GlueWidget(this.properties, {super.key});
}
