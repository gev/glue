# Native Properties Implementation Plan

## Overview
Implement property access for native values in Glue, enabling object-oriented programming with getters only. This extends `Value` with functional property descriptors that support computed properties, validation, and FRP capabilities. Mutable state is handled by the host platform, not Glue.

## Design Specification

### Value Extension
```haskell
data Value = Value {
    getValue :: Dynamic,      -- Opaque host object
    getters :: Map Text (IR m)    -- Property getter functions only
}
```

### Property Operations
- **Access**: `obj.prop` → calls getter function
- **Methods**: Getters can return callable functions
- **No Assignment**: Properties are read-only from Glue; mutation handled by host

## Haskell Implementation Plan

### Haskell Source Changes
To be modified in: `haskell/glue/src/Glue/`

| Haskell Module | Changes Required |
|----------------|------------------|
| `IR.hs` | Update Value definition, add property evaluation logic |
| `Eval.hs` | Modify DottedSymbol evaluation for property access/setting |

### Haskell Test Updates
To be modified in: `haskell/glue/test/`

| Haskell Test | Changes Required |
|--------------|------------------|
| `EvalTest.hs` | Add property access/setting tests |

## Dart Implementation Plan

### Dart Source Changes
To be modified in: `dart/glue/lib/src/`

| Dart Module | Changes Required |
|----------------|------------------|
| `ir.dart` | Update Value class, add property evaluation logic |
| `eval.dart` | Modify DottedSymbol evaluation for property access/setting |

### Dart Test Updates
To be modified in: `dart/glue/test/`

| Dart Test | Changes Required |
|-----------|------------------|
| `native_test.dart` | Add property access/setting tests |

## Implementation Strategy

### Phase 1: Core Property System
**Priority: High** - Basic property access (read-only)

1. **Value Extension** (2 modules)
   - [ ] Update `Value` definition in Haskell and Dart (getters only)
   - [ ] Modify constructors to accept getters map only
   - [ ] Update utility functions (`hostValue`, `extractValue`, etc.)

2. **Property Access** (2 modules)
   - [ ] Modify `DottedSymbol` evaluation for getter calls
   - [ ] Add property lookup logic in IR evaluation

### Phase 2: Read-Only Properties
**Priority: High** - Complete getter-only functionality

3. **Property Access Logic** (2 modules)
   - [ ] Add getter lookup and invocation
   - [ ] Handle getter errors (missing property, wrong arguments)

### Phase 3: Testing & Integration
**Priority: High** - Verify complete functionality

4. **Unit Tests** (2 test modules)
   - [ ] Test property access on native objects
   - [ ] Test error cases (missing properties, etc.)

5. **Integration Tests** (2 implementations)
   - [ ] Test property operations in full evaluation context
   - [ ] Verify FRP capabilities with reactive properties

## Haskell/Dart Correspondence Table

| Feature | Haskell Implementation | Dart Implementation | Status |
|---------|------------------------|---------------------|--------|
| **Value Extension** | Update IR.hs | Update ir.dart | ⏳ PENDING |
| **Property Access** | Modify Eval.hs | Modify eval.dart | ⏳ PENDING |
| **Tests** | Update EvalTest.hs | Update native_test.dart | ⏳ PENDING |

## Implementation Requirements

### Structural Compliance
- **Type Definitions**: Value must be identical in both implementations
- **Evaluation Logic**: Property access behavior must match
- **Error Handling**: Same error conditions in both implementations

### Behavioral Fidelity
- **Getter Calls**: `obj.prop` evaluates getter function
- **Function Application**: Getters called with correct arguments
- **Error Propagation**: Missing properties, wrong argument counts, etc.

### Testing Requirements
- **Property Access**: Test getters return correct values
- **Error Cases**: Test missing properties, invalid operations
- **FRP Support**: Test reactive property behaviors

## Success Criteria

- [ ] **Value extended** with getters in both implementations
- [ ] **Property access works**: `obj.prop` calls getter function
- [ ] **Error handling**: Proper errors for missing/invalid properties
- [ ] **Tests pass**: Comprehensive test coverage for property operations
- [ ] **FRP ready**: Foundation supports reactive programming

## Timeline Estimate

- **Phase 1**: 1 week (Value extension + property access)
- **Phase 2**: 1 week (testing + integration)

**Total: 2 weeks for complete native properties implementation**

## Risk Assessment

- **Evaluation Complexity**: Modifying DottedSymbol evaluation logic
- **Function Calling**: Ensuring getters are called correctly
- **Cross-Implementation**: Maintaining identical behavior in Haskell/Dart

## Dependencies

- **Core Evaluation**: Must support function application for getters
- **IR System**: Value integration with IR types
- **Test Framework**: Comprehensive testing of property operations

## Property Operations Summary

### Getter Functions:
- Take 0 arguments (property access)
- Return property value as IR
- Can be computed, cached, or reactive

### Syntax Support:
- `obj.prop` → Property access via getter
- `obj.method()` → Method calls via getter returning function
