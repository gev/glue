import 'package:flutter/widgets.dart';
import 'package:glue/ir.dart';
import 'package:glue/runtime.dart';
import 'reactive_helpers.dart';

/// Reactive widget that re-evaluates child expression on each build
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

  @override
  Widget build(BuildContext context) {
    // Re-evaluate child expression on each reactive update
    final result = evalSync(childExpr, runtime);
    return extractWidget(result) ?? const SizedBox();
  }
}

/// Synchronous evaluation helper for reactive widgets
/// This is a simplified version - in practice, we'd need async evaluation
/// For now, we'll evaluate simple expressions synchronously
Ir evalSync(Ir expression, Runtime runtime) {
  // For widget creation expressions, try to evaluate synchronously
  // This is a placeholder - real implementation would need proper sync evaluation
  switch (expression) {
    case IrList(elements: [IrSymbol(value: 'text'), ...]):
      // For text widgets, we could evaluate synchronously
      // But for now, return a placeholder
      return IrNativeValue(
        HostValue(
          Container(
            padding: const EdgeInsets.all(8),
            child: const Text('Reactive Text - TODO: Implement sync eval'),
          ),
        ),
      );
    case IrNativeValue(value: HostValue(value: Widget _)):
      return expression;
    default:
      // For complex expressions, return placeholder
      return IrNativeValue(
        HostValue(
          Container(
            padding: const EdgeInsets.all(8),
            child: Text('Reactive Widget: ${expression.toString()}'),
          ),
        ),
      );
  }
}
