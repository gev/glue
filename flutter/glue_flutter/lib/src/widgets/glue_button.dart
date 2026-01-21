import 'package:flutter/material.dart';
import 'glue_widget.dart';

/// Glue Button widget - Flutter implementation of interactive button
class GlueButton extends GlueWidget {
  const GlueButton(super.properties);

  @override
  Widget build(BuildContext context) {
    final label = properties['label'];
    final onTap = properties['on-tap'];
    final disabled = properties['disabled'];

    final String buttonText = label is String ? label : 'Button';
    final bool isDisabled = disabled is bool ? disabled : false;

    return ElevatedButton(
      onPressed: isDisabled ? null : (_handleTap(onTap)),
      child: Text(buttonText),
    );
  }

  VoidCallback? _handleTap(dynamic onTap) {
    // For now, just return null - event handling needs more work
    // TODO: Implement callback extraction from IrClosure
    return null;
  }
}
