import 'package:flutter/material.dart';
import 'package:glue_flutter/src/widgets/glue_widget.dart';

/// Glue Button widget - Flutter implementation of interactive button
class GlueButton extends GlueWidget {
  final String label;
  final VoidCallback? onPressed;
  final bool disabled;

  const GlueButton({
    required this.label,
    this.onPressed,
    this.disabled = false,
    super.key,
  });

  @override
  Widget build(BuildContext context) {
    return ElevatedButton(
      onPressed: disabled ? null : onPressed,
      child: Text(label),
    );
  }
}
