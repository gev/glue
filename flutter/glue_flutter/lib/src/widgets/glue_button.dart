import 'package:flutter/material.dart';
import 'glue_widget.dart';

/// A button widget that can be created from Glue code expressions.
///
/// Example Glue usage:
/// ```clojure
/// (button :label "Click me" :on-tap (lambda () (print "Clicked!")))
/// ```
class GlueButton extends GlueWidget {
  final String label;
  final VoidCallback? onPressed;

  const GlueButton({super.key, required this.label, this.onPressed});

  @override
  Widget build(BuildContext context) {
    return ElevatedButton(onPressed: onPressed, child: Text(label));
  }
}
