# Test Coverage Analysis Process Documentation

## Overview

This document describes the systematic process for creating detailed test coverage reports for any language implementation of the Glue programming language. The analysis follows a structured 8-step methodology that uses ModuleInfo to ensure comprehensive and accurate coverage assessment across all implementations.

## Process Methodology

### Phase 1: Directory Structure Analysis

#### Step 1: Get Directory Tree
- **Input**: Existing `directory-structure.md` files in `haskell/` and `dart/` folders
- **Process**: Parse complete directory trees to extract all module and test file inventories
- **Output**: Structured lists of implementation modules and test files for both languages

#### Step 2: Extract ModuleInfo from Implementation Files
- **Input**: Individual implementation files (.hs/.dart)
- **Process**: Extract ModuleInfo using two complementary methods:
  - **Method A - Code Comments**: Parse "Mirrors Haskell [ModuleName]" comments
  - **Method B - File Path Structure**: Derive from file path patterns
- **Output**: Complete ModuleInfo mapping for all implementation files

**ModuleInfo Extraction Details:**

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
1. Parse file path: `lib/src/lib/[path]/[filename].dart`
2. Extract path segments after `lib/src/lib/`
3. Capitalize each segment: `bool` → `Bool`, `eq` → `Eq`
4. Construct: `Glue.[Path1].[Path2].[Filename]`
5. Verify with comment cross-reference

**Examples:**
```
lib/src/lib/bool/eq.dart → Glue.Lib.Bool.Eq
lib/src/lib/list/append.dart → Glue.Lib.List.Append
lib/src/lib/math/arithmetic/add.dart → Glue.Lib.Math.Arithmetic.Add
lib/ast.dart → Glue.Ast (core modules)
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
- **Output**: Pivot table rows with coverage data

## Pivot Table Structure

### Core Pivot Table (Primary Analysis Table)
```
| Category | Haskell Modules | Dart Modules | Test Coverage | Status | Details |
|----------|-----------------|--------------|----------------|--------|---------|
| Core Language | 12 files | 16 files | X/Y tests | ✅ Complete | AST, IR, Parser, Environment, Evaluation, Runtime, Error Handling, Either Monad |
| Bool Library | 13 files | 13 files | X/Y tests | ✅ Complete | 12 functions + main module |
| Builtin Library | 8 files | 8 files | X/Y tests | ⚠️ Partial | 7 functions + main module (missing: error_test.dart, let_test.dart) |
| IO Library | 3 files | 3 files | X/Y tests | ✅ Complete | print, println, read functions |
| List Library | 22 files | 22 files | X/Y tests | ✅ Complete | 21 functions + main module |
| Math Library | 23 files | 6 files | X/Y tests | ⚠️ Partial | arithmetic submodule complete, others test-only |
| Module System | 5 files | 3 files | X/Y tests | ⚠️ Partial | Cache, Registry complete; Error, Loader missing |
```

### Library-Specific Pivot Tables

Each library path gets its own detailed table:

#### Bool Library Table
```
| Module Path | Haskell File | Dart File | Test File | Test Count | Status |
|-------------|--------------|-----------|-----------|------------|--------|
| lib.bool.eq | Eq.hs | eq.dart | eq_test.dart | 5 | ✅ Complete |
| lib.bool.ge | Ge.hs | ge.dart | ge_test.dart | 5 | ✅ Complete |
| ...etc for all 12 bool modules
```

#### Builtin Library Table
```
| Module Path | Haskell File | Dart File | Test File | Test Count | Status |
|-------------|--------------|-----------|------------|------------|--------|
| lib.builtin.def | Def.hs | def.dart | def_test.dart | 7 | ✅ Complete |
| lib.builtin.lambda | Lambda.hs | lambda.dart | lambda_test.dart | 7 | ✅ Complete |
| lib.builtin.let | Let.hs | let.dart | N/A | N/A | ✅ Complete |
| ...etc for all 8 builtin modules
```

#### List Library Table
```
| Module Path | Haskell File | Dart File | Test File | Test Count | Status |
|-------------|--------------|-----------|-----------|------------|--------|
| lib.list.append | Append.hs | append.dart | append_test.dart | 8 | ✅ Complete |
| lib.list.car | Car.hs | car.dart | car_test.dart | 5 | ✅ Complete |
| ...etc for all 21 list functions + main
```

#### Math Submodule Tables
```
| Module Path | Haskell File | Dart File | Test File | Test Count | Status |
|-------------|--------------|-----------|-----------|------------|--------|
| lib.math.arithmetic.add | Add.hs | add.dart | add_test.dart | 6 | ✅ Complete |
| lib.math.arithmetic.div | Div.hs | div.dart | div_test.dart | 6 | ✅ Complete |
| ...etc for all arithmetic modules
```

## Coverage Metrics Methodology

### Test Counting Methods

#### Haskell Test Counting
- **Method**: Count `it` blocks in HSpec test files
- **Example**: Each `it "description" $ do` represents one test
- **Validation**: Cross-check with actual test execution

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
- `haskell/directory-structure.md` - Haskell reference structure
- `dart/directory-structure.md` - Dart implementation structure

### Output Files
- `haskell/test-coverage.md` - Haskell test coverage analysis
- `dart/test-coverage.md` - Dart test coverage analysis
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

### Haskell Test Coverage Report
- Complete analysis of 22 test files
- Detailed coverage for core language + 5 libraries
- Identification of any missing Haskell tests

### Dart Test Coverage Report
- Complete analysis of 38+ test files
- Detailed coverage for core language + 5 libraries
- Clear identification of missing tests (error_test.dart, let_test.dart)

### Comparative Insights
- Coverage comparison between Haskell and Dart
- Identification of implementation differences
- Recommendations for test coverage improvements

This systematic methodology ensures comprehensive, accurate, and actionable test coverage analysis for both implementations.
