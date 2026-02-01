import 'package:flutter/material.dart';
import 'package:glue/eval.dart';
import 'package:glue/ir.dart';
import 'package:glue_flutter/src/utils/widget_properties.dart';

/// Stepper widget function
/// Creates Flutter Stepper from Glue (stepper props) expressions
final Ir stepper = IrNativeFunc(stepperImpl);

/// Stepper implementation - takes properties object
Eval<Ir> stepperImpl(Ir props) => switch (props) {
  IrObject(:final properties) => _createStepper(
    WidgetProperties(properties.unlock),
  ),
  _ => _createStepper(WidgetProperties.empty()),
};

/// Create Stepper widget from properties
Eval<Ir> _createStepper(WidgetProperties properties) {
  return getRuntime().map((runtime) {
    final stepperWidget = Stepper(
      key: properties.key,
      steps: properties.getValues<Step>('steps'),
      currentStep: properties.getInt('current-step') ?? 0,
      onStepTapped: properties.getValue<Function(int)?>('on-step-tapped'),
      onStepContinue: properties
          .getVoidCallback('on-step-continue')
          ?.call(runtime),
      onStepCancel: properties.getVoidCallback('on-step-cancel')?.call(runtime),
      controlsBuilder: properties
          .getValue<Widget Function(BuildContext, ControlsDetails)>(
            'controls-builder',
          ),
      type: properties.getValue<StepperType>('type') ?? StepperType.vertical,
      physics: properties.getValue<ScrollPhysics>('physics'),
      elevation: properties.getDouble('elevation'),
      margin: properties.getValue<EdgeInsetsGeometry?>('margin'),
      connectorColor: properties.getValue<WidgetStateProperty<Color>?>(
        'connector-color',
      ),
      connectorThickness: properties.getDouble('connector-thickness'),
      stepIconBuilder: properties.getValue<Widget Function(int, StepState)>(
        'step-icon-builder',
      ),
    );
    return IrNativeValue(Value(stepperWidget));
  });
}
