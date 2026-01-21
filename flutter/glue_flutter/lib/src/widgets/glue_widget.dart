import 'package:flutter/material.dart';

/// Base class for all Glue-powered Flutter widgets.
///
/// This abstract class provides the foundation for widgets that can be created
/// and configured through Glue code expressions.
abstract class GlueWidget extends StatelessWidget {
  const GlueWidget({super.key});
}

/// Registry for managing Glue widget creation and types
class GlueWidgetRegistry {
  static final Map<String, GlueWidget Function(Map<String, dynamic>)>
  _registry = {};

  /// Registers a widget factory function
  static void register(
    String type,
    GlueWidget Function(Map<String, dynamic>) factory,
  ) {
    _registry[type] = factory;
  }

  /// Creates a widget from type and properties
  static GlueWidget? create(String type, Map<String, dynamic> properties) {
    final factory = _registry[type];
    return factory?.call(properties);
  }

  /// Gets all registered widget types
  static Set<String> get registeredTypes => _registry.keys.toSet();
}
