import 'package:flutter/material.dart';
import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/runtime.dart';
import 'package:glue_demo/services/state/state_helpers.dart';

/// Reactive widget that caches previous result and shows it while updating
class ListenableWidget extends StatefulWidget {
  final ChangeNotifier notifier;
  final Ir childExpr;
  final Runtime runtime;

  const ListenableWidget({
    required this.notifier,
    required this.childExpr,
    required this.runtime,
    super.key,
  });

  @override
  State<ListenableWidget> createState() => _ListenableWidgetState();
}

class _ListenableWidgetState extends State<ListenableWidget> {
  Widget _cachedWidget = CircularProgressIndicator();

  @override
  void initState() {
    super.initState();
    widget.notifier.addListener(_updateWidget);
    _updateWidget(); // Initial evaluation
  }

  @override
  void didUpdateWidget(ListenableWidget oldWidget) {
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
      (error) => _cachedWidget,
      (value) => extractWidget(value.$1),
    );
    if (mounted) {
      setState(() => _cachedWidget = newWidget);
    }
  }

  @override
  Widget build(BuildContext context) {
    return _cachedWidget;
  }
}
