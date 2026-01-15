# Test Coverage Analysis Process Documentation

## Overview

This document describes the systematic process for creating detailed test coverage reports for a language implementation of the Glue programming language. The analysis follows a structured 8-step methodology that uses ModuleInfo to ensure comprehensive and accurate coverage assessment.

**Related Documents:**
- [Haskell Test Coverage Analysis](haskell/test-coverage.md)
- [Dart Test Coverage Analysis](dart/test-coverage.md)
- [Haskell vs Dart Comparison Methodology](haskell-dart-comparison-methodology.md)

## Process Methodology

### Phase 1: Directory Structure Analysis

#### Step 1: Get Directory Tree
- **Input**: Existing `directory-structure.md` file for the implementation
- **Process**: Parse complete directory tree to extract all module and test file inventories
- **Output**: Structured lists of implementation modules and test files

#### Step 2: Extract ModuleInfo from Implementation Files
- **Input**: Individual implementation files (.hs/.dart)
- **Process**: Extract ModuleInfo using two complementary methods:
  - **Method A - Code Comments**: Parse "Mirrors Haskell [ModuleName]" comments
  - **Method B - File Path Structure**: Derive from file path patterns
- **Output**: Complete ModuleInfo mapping for all implementation files

**ModuleInfo Extraction Details:**

ModuleInfo produces Glue module dotted names that identify the corresponding Glue standard library module.

**Source 1: Code Comments**
```dart
/// Mirrors Haskell Glue.Lib.Bool.Eq.eq exactly
/// Mirrors Haskell Glue.Lib.List.Append.append exactly
```

**Source 2: File Path Structure**
```
lib/src/lib/[module]/[function].dart
           └───┬──┘ └───┬────┘
               │        └─ Function name
               └─ Module path
```

**ModuleInfo Construction Algorithm:**
1. Parse mirror comment to extract Haskell module path (e.g., Glue.Lib.Bool.Eq)
2. Remove "Glue." prefix: Lib.Bool.Eq
3. Lowercase all segments: lib.bool.eq
4. Remove "lib." prefix if present: bool.eq
5. For core modules, use the module name directly (e.g., Glue.Ast → ast)
6. Verify with file path cross-reference

**Examples:**
```
lib/src/lib/bool/eq.dart → bool
lib/src/lib/list/append.dart → list
lib/src/lib/math/arithmetic/add.dart → math.arithmetic
lib/ast.dart → ast
```

#### Step 3: Get Tests Subtree
- **Input**: Directory structure data + ModuleInfo mapping
- **Process**: Extract test file subtrees and map them to ModuleInfo
- **Output**: Test file inventories with ModuleInfo-test relationships

#### Step 4: Compare Module vs Test Structures
- **Input**: ModuleInfo mapping + test inventories
- **Process**: Create correspondence matrices showing which ModuleInfo have tests and which don't
- **Output**: Coverage mapping tables identifying gaps and mismatches

### Phase 2: Module-by-Module Analysis

#### Step 5: Open Every Module
- **Input**: Individual implementation files (.hs/.dart)
- **Process**:
  - Examine function signatures and implementations
  - Identify complexity factors requiring comprehensive testing
  - Document edge cases and error conditions
- **Output**: Implementation analysis for coverage requirements

#### Step 6: Open Every Test
- **Input**: Individual test files (Spec.hs/_test.dart)
- **Process**:
  - Count actual test functions (`it` blocks for Haskell, `test()` calls for Dart)
  - Analyze test quality and completeness
  - Verify test-implementation alignment
- **Output**: Test file analysis with coverage metrics

#### Step 7: Compare Coverage Quality
- **Input**: Implementation analysis + test analysis
- **Process**:
  - Assess if tests adequately cover implementation functionality
  - Identify under-tested areas and missing test scenarios
  - Evaluate overall coverage completeness
- **Output**: Coverage quality assessment reports

#### Step 8: Create Pivot Table Rows
- **Input**: Coverage analysis results
- **Process**: Generate metrics and status indicators for each module
- **Output**: Pivot and detailed table rows with coverage data

## Pivot Table Structure

### One Pivot Table (Primary Analysis Table)

| Module | Files | Test Coverage | Status | Details |
|--------|-------|----------------|--------|---------|
| core | 7 files | X/Y tests | ✅ Complete | Core language modules (AST, IR, Parser, Environment, Evaluation, Runtime, Module System) |
| bool | 13 files | X/Y tests | ✅ Complete | Boolean operations library |
| builtin | 8 files | X/Y tests | ⚠️ Partial | Builtin operations library |
| io | 3 files | X/Y tests | ✅ Complete | IO operations library |
| list | 22 files | X/Y tests | ✅ Complete | List operations library |
| math.arithmetic | 6 files | X/Y tests | ✅ Complete | Arithmetic operations |
| math.const | 1 file | X/Y tests | ⚠️ Partial | Math constants |
| math.power | 4 files | X/Y tests | ⚠️ Partial | Power operations |
| ...etc for all Glue modules

### Detailed Tables

One detailed table per Glue module dotted name, with title including the dotted name.

#### Core Detailed Table

| Function | Module Name | Test Name | Coverage | Status |
|----------|-------------|-----------|----------|--------|
| ast | ast.dart | ast_test.dart | 10 | ✅ Complete |
| ir | ir.dart | ir_test.dart | 8 | ✅ Complete |
| parser | parser.dart | parser_test.dart | 12 | ✅ Complete |
| ...etc for all core modules

Where:
- **Function**: The Glue function or module name being tested
- **Module Name**: The host language implementation file name
- **Test Name**: The test file name
- **Coverage**: Number of test cases for this function/module
- **Status**: Test coverage status (Complete, Partial, etc.)

#### Bool Detailed Table

| Function | Module Name | Test Name | Coverage | Status |
|----------|-------------|-----------|----------|--------|
| eq | eq.dart | eq_test.dart | 5 | ✅ Complete |
| ge | ge.dart | ge_test.dart | 5 | ✅ Complete |
| gt | gt.dart | gt_test.dart | 5 | ✅ Complete |
| ...etc for all bool functions

Where:
- **Function**: The Glue function or module name being tested
- **Module Name**: The host language implementation file name
- **Test Name**: The test file name
- **Coverage**: Number of test cases for this function/module
- **Status**: Test coverage status (Complete, Partial, etc.)

#### List Detailed Table

| Function | Module Name | Test Name | Coverage | Status |
|----------|-------------|-----------|----------|--------|
| append | append.dart | append_test.dart | 8 | ✅ Complete |
| car | car.dart | car_test.dart | 5 | ✅ Complete |
| cdr | cdr.dart | cdr_test.dart | 5 | ✅ Complete |
| ...etc for all list functions

Where:
- **Function**: The Glue function or module name being tested
- **Module Name**: The host language implementation file name
- **Test Name**: The test file name
- **Coverage**: Number of test cases for this function/module
- **Status**: Test coverage status (Complete, Partial, etc.)


#### Math.Arithmetic Detailed Table

| Function | Module Name | Test Name | Coverage | Status |
|----------|-------------|-----------|----------|--------|
| add | add.dart | add_test.dart | 6 | ✅ Complete |
| sub | sub.dart | sub_test.dart | 6 | ✅ Complete |
| mul | mul.dart | mul_test.dart | 6 | ✅ Complete |
| ...etc for all math.arithmetic functions

Where:
- **Function**: The Glue function or module name being tested
- **Module Name**: The host language implementation file name
- **Test Name**: The test file name
- **Coverage**: Number of test cases for this function/module
- **Status**: Test coverage status (Complete, Partial, etc.)


...etc for each Glue module dotted name (builtin, io, math.const, math.power, etc.)

## Coverage Metrics Methodology

### Test Counting Methods

#### Haskell Test Counting
- **Method**: Count all HSpec test constructs (`it`, `prop`, `describe`, etc.) in test files
- **Example**: Each `it "description" $ do` and each `prop "description"` represents one test case
- **Validation**: Cross-check with actual test execution output showing total examples
- **Note**: QuickCheck properties provide additional test coverage through generated test cases, but each property counts as one HSpec test case

#### Dart Test Counting
- **Method**: Count `test()` function calls in test files
- **Example**: Each `test('description', () { ... })` represents one test
- **Validation**: Manual inspection of test files

### Status Indicators

- ✅ **Complete**: Module exists + adequate test coverage + tests pass
- ⚠️ **Partial**: Module exists + some test coverage + gaps identified
- ❌ **Missing**: Module exists + no corresponding test file
- 🔍 **Test-only**: Test exists + no implementation module (Math library case)

## File Organization

### Input Files
- `directory-structure.md` - Implementation directory structure

### Output Files
- `test-coverage.md` - Test coverage analysis
- `context/test-coverage-analysis-process.md` - This process documentation

## Quality Assurance

### Verification Steps
1. **File Path Verification**: Cross-reference all file paths with directory structures
2. **Test Count Validation**: Manual inspection of test files to verify counts
3. **Status Accuracy**: Ensure status indicators reflect actual coverage quality
4. **Consistency Check**: Verify table formatting and data consistency

### Accuracy Metrics
- **File References**: 100% accuracy against directory structures
- **Test Counts**: Verified by manual inspection
- **Status Indicators**: Based on comprehensive analysis
- **Table Completeness**: All modules accounted for

## Expected Outcomes

### Test Coverage Report
- Complete analysis of test files
- Detailed coverage for core language + libraries
- Identification of any missing tests

This systematic methodology ensures comprehensive, accurate, and actionable test coverage analysis for the implementation.
