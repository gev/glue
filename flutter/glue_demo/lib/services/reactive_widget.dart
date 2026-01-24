import 'package:flutter/material.dart';
import 'package:glue/ir.dart';
import 'package:glue/eval.dart';
import 'package:glue/runtime.dart';
import 'reactive_helpers.dart';

/// Reactive widget that re-evaluates child expression on each reactive update
class ReactiveWidget extends StatelessWidget {
  final ChangeNotifier notifier;
  final Ir childExpr;
  final Runtime runtime;

  const ReactiveWidget({
    required this.notifier,
    required this.childExpr,
    required this.runtime,
    super.key,
  });

  Future<Ir> _evaluate() async {
    final result = await runEval(eval(childExpr), runtime);
    return result.match(
      (error) => IrString('Error: $error'),
      (value) => value.$1,
    );
  }

  @override
  Widget build(BuildContext context) {
    return ListenableBuilder(
      listenable: notifier,
      builder: (context, _) => FutureBuilder<Ir>(
        future: _evaluate(),
        builder: (context, snapshot) {
          if (snapshot.connectionState == ConnectionState.waiting) {
            return const CircularProgressIndicator();
          }

          final result = snapshot.data;
          if (result == null) return const Text('No result');

          return extractWidget(result) ?? Text('Result: ${result.toString()}');
        },
      ),
    );
  }
}
