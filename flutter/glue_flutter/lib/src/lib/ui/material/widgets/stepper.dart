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
  final stepperWidget = Stepper(
    steps: properties.stepperSteps ?? [],
    currentStep: properties.stepperCurrentStep,
    onStepTapped: properties.stepperOnStepTapped,
    onStepContinue: properties.stepperOnStepContinue,
    onStepCancel: properties.stepperOnStepCancel,
    controlsBuilder: properties.stepperControlsBuilder,
    type: properties.stepperType,
    physics: properties.stepperPhysics,
    elevation: properties.stepperElevation,
    margin: properties.stepperMargin,
    connectorColor: properties.stepperConnectorColor,
    connectorThickness: properties.stepperConnectorThickness,
    stepIconBuilder: properties.stepperStepIconBuilder,
  );
  return Eval.pure(IrNativeValue(Value(stepperWidget)));
}
