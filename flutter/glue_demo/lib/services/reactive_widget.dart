import 'package:flutter/material.dart';
import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/runtime.dart';
import 'reactive_helpers.dart';

/// Reactive widget that caches previous result and shows it while updating
class ReactiveWidget extends StatefulWidget {
  final ChangeNotifier notifier;
  final Ir childExpr;
  final Runtime runtime;

  const ReactiveWidget({
    required this.notifier,
    required this.childExpr,
    required this.runtime,
    super.key,
  });

  @override
  State<ReactiveWidget> createState() => _ReactiveWidgetState();
}

class _ReactiveWidgetState extends State<ReactiveWidget> {
  Widget? _cachedWidget;

  @override
  void initState() {
    super.initState();
    widget.notifier.addListener(_updateWidget);
    _updateWidget(); // Initial evaluation
  }

  @override
  void didUpdateWidget(ReactiveWidget oldWidget) {
    super.didUpdateWidget(oldWidget);
    if (oldWidget.notifier != widget.notifier) {
      oldWidget.notifier.removeListener(_updateWidget);
      widget.notifier.addListener(_updateWidget);
    }
  }

  @override
  void dispose() {
    widget.notifier.removeListener(_updateWidget);
    super.dispose();
  }

  void _updateWidget() async {
    // Keep showing old widget while calculating new one
    final result = await runEval(eval(widget.childExpr), widget.runtime);
    final newWidget = result.match(
      (error) => _cachedWidget ?? const SizedBox(), // Keep old on error
      (value) => extractWidget(value.$1) ?? _cachedWidget ?? const SizedBox(),
    );
    if (mounted) {
      setState(() => _cachedWidget = newWidget);
    }
  }

  @override
  Widget build(BuildContext context) {
    return _cachedWidget ??
        const CircularProgressIndicator(); // Only on first load
  }
}
