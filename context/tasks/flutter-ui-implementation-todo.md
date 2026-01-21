# Flutter UI Module Implementation TODO

## Prerequisites
- [ ] **Read and understand [context/development-rules.md](../development-rules.md)** - All implementation must follow Glue development rules
- [ ] Read [context/ui-module-specification.md](../ui-module-specification.md) - Framework-agnostic UI API specification
- [ ] Read [flutter/glue_flutter/README.md](../../flutter/glue_flutter/README.md) - Package documentation and structure

## Overview
Implement the Flutter-specific version of the `ui` module for the Glue programming language. This provides Flutter widget implementations for the framework-agnostic UI functions defined in the `ui` module specification.

## Current Status
- ✅ ui-module-specification.md cleaned and corrected
- ✅ flutter/glue_flutter/README.md created

## Implementation Requirements

### Module Structure Setup
- [ ] Create flutter/glue_flutter/lib/src/ directory
- [ ] Create flutter/glue_flutter/lib/src/ui.dart module file
- [ ] Set up nativeModule('ffi.ui', [...]) structure with function exports

### Widget Classes Implementation
- [ ] Create GlueText widget class extending GlueWidget
- [ ] Create GlueButton widget class extending GlueWidget
- [ ] Create GlueContainer widget class extending GlueWidget
- [ ] Create GlueColumn widget class extending GlueWidget
- [ ] Create GlueRow widget class extending GlueWidget
- [ ] Create GluePadding widget class extending GlueWidget
- [ ] Create GlueCenter widget class extending GlueWidget

### Core Native Functions
- [ ] Implement `text` native function: (text content props) → IrNativeValue
- [ ] Implement `button` native function: (button props) → IrNativeValue
- [ ] Implement `container` native function: (container props) → IrNativeValue
- [ ] Implement `column` native function: (column props) → IrNativeValue
- [ ] Implement `row` native function: (row props) → IrNativeValue
- [ ] Implement `padding` native function: (padding child props) → IrNativeValue
- [ ] Implement `center` native function: (center child) → IrNativeValue

### Property Parsing Utilities
- [ ] Create color parsing function: string → Color
- [ ] Create font weight parsing: string → FontWeight
- [ ] Create text alignment parsing: string → TextAlign
- [ ] Create main axis alignment parsing: string → MainAxisAlignment
- [ ] Create cross axis alignment parsing: string → CrossAxisAlignment
- [ ] Create edge insets parsing: various → EdgeInsets

### Parameter Validation
- [ ] Add validation for required parameters (content, label, children)
- [ ] Add type checking for property values
- [ ] Add meaningful error messages for invalid inputs
- [ ] Handle optional parameters with defaults

### Event Handling
- [ ] Implement callback function extraction from IrObject
- [ ] Create wrapper functions for Flutter event handlers
- [ ] Support on-tap, on-press, and other event callbacks

### Testing
- [ ] Create flutter/glue_flutter/test/ui_test.dart
- [ ] Add unit tests for each native function
- [ ] Test parameter parsing and validation
- [ ] Test widget creation and property application
- [ ] Test error handling for invalid inputs

### Integration
- [ ] Update flutter/glue_flutter/lib/glue_flutter.dart exports
- [ ] Ensure module is properly registered in Glue environment
- [ ] Test integration with Glue evaluator

## Syntax Requirements
Widget functions follow pattern: (function content? props?)
- Text: (text "content" (:color "blue" :size 18 :weight "bold"))
- Button: (button (:label "Click" :on-tap callback))
- Container: (container (:children [child1 child2] :direction "vertical"))

## Success Criteria
- [ ] All 7 core widget functions implemented and working
- [ ] Proper property parsing for all supported parameters
- [ ] Error handling for invalid inputs
- [ ] Full test coverage for all functions
- [ ] Integration with Glue evaluator confirmed
- [ ] Documentation complete and accurate

## Dependencies
- glue: Core Glue interpreter
- flutter/material.dart: Flutter widget framework

## Implementation Notes
- All widgets return IrNativeValue(HostValue(FlutterWidget)) without getters/setters
- Property objects parsed from IrObject with key-value pairs
- Functions are native (IrNativeFunc) taking single Ir argument (universal currying)
- Module name is "ui" (not "ffi.ui" as originally planned)
