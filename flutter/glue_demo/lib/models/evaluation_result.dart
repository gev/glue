/// Models for Glue evaluation results
library;

import 'package:flutter/material.dart';

/// Result of Glue code evaluation
class EvaluationResult {
  final List<Widget>? widgets;
  final String? errorMessage;
  final String? stackTrace;
  final bool isSuccess;

  EvaluationResult._({
    this.widgets,
    this.errorMessage,
    this.stackTrace,
    required this.isSuccess,
  });

  factory EvaluationResult.success(List<Widget> widgets) {
    return EvaluationResult._(widgets: widgets, isSuccess: true);
  }

  factory EvaluationResult.error(String errorMessage, String stackTrace) {
    return EvaluationResult._(
      errorMessage: errorMessage,
      stackTrace: stackTrace,
      isSuccess: false,
    );
  }
}
