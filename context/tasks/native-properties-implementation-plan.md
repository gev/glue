# Native Properties Implementation Plan

## Overview
Implement property access and assignment for native values in Glue, enabling object-oriented programming with getters and setters. This extends `HostValue` with functional property descriptors that support computed properties, validation, and FRP capabilities.

## Design Specification

### HostValue Extension
```haskell
data HostValue = HostValue {
    getHostValue :: Dynamic,      -- Opaque host object
    getters :: Map Text (IR m),   -- Property getter functions
    setters :: Map Text (IR m)    -- Property setter functions
}
```

### Property Operations
- **Access**: `obj.prop` → calls getter function
- **Assignment**: `(set obj.prop value)` → calls setter function
- **Methods**: Getters can return callable functions

## Haskell Implementation Plan

### Haskell Source Changes
To be modified in: `haskell/glue/src/Glue/`

| Haskell Module | Changes Required |
|----------------|------------------|
| `IR.hs` | Update HostValue definition, add property evaluation logic |
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
| `ir.dart` | Update HostValue class, add property evaluation logic |
| `eval.dart` | Modify DottedSymbol evaluation for property access/setting |

### Dart Test Updates
To be modified in: `dart/glue/test/`

| Dart Test | Changes Required |
|-----------|------------------|
| `native_test.dart` | Add property access/setting tests |

## Implementation Strategy

### Phase 1: Core Property System
**Priority: High** - Basic property access and assignment

1. **HostValue Extension** (2 modules)
   - [ ] Update `HostValue` definition in Haskell and Dart
   - [ ] Modify constructors to accept getters/setters maps
   - [ ] Update utility functions (`hostValue`, `extractHostValue`, etc.)

2. **Property Access** (2 modules)
   - [ ] Modify `DottedSymbol` evaluation for getter calls
   - [ ] Add property lookup logic in IR evaluation

### Phase 2: Property Assignment
**Priority: High** - Complete setter functionality

3. **Set Special Form Extension** (2 modules)
   - [ ] Extend `set` evaluation to handle `DottedSymbol` targets
   - [ ] Implement setter function calls

4. **Property Setting Logic** (2 modules)
   - [ ] Add setter lookup and invocation
   - [ ] Handle setter errors (missing property, wrong arguments)

### Phase 3: Testing & Integration
**Priority: High** - Verify complete functionality

5. **Unit Tests** (2 test modules)
   - [ ] Test property access on native objects
   - [ ] Test property assignment
   - [ ] Test error cases (missing properties, etc.)

6. **Integration Tests** (2 implementations)
   - [ ] Test property operations in full evaluation context
   - [ ] Verify FRP capabilities with reactive properties

## Haskell/Dart Correspondence Table

| Feature | Haskell Implementation | Dart Implementation | Status |
|---------|------------------------|---------------------|--------|
| **HostValue Extension** | Update IR.hs | Update ir.dart | ⏳ PENDING |
| **Property Access** | Modify Eval.hs | Modify eval.dart | ⏳ PENDING |
| **Property Setting** | Extend set evaluation | Extend set evaluation | ⏳ PENDING |
| **Tests** | Update EvalTest.hs | Update native_test.dart | ⏳ PENDING |

## Implementation Requirements

### Structural Compliance
- **Type Definitions**: HostValue must be identical in both implementations
- **Evaluation Logic**: Property access/setting behavior must match
- **Error Handling**: Same error conditions in both implementations

### Behavioral Fidelity
- **Getter Calls**: `obj.prop` evaluates getter function
- **Setter Calls**: `(set obj.prop value)` evaluates setter function
- **Function Application**: Getters/setters called with correct arguments
- **Error Propagation**: Missing properties, wrong argument counts, etc.

### Testing Requirements
- **Property Access**: Test getters return correct values
- **Property Setting**: Test setters update state correctly
- **Error Cases**: Test missing properties, invalid operations
- **FRP Support**: Test reactive property behaviors

## Success Criteria

- [ ] **HostValue extended** with getters/setters in both implementations
- [ ] **Property access works**: `obj.prop` calls getter function
- [ ] **Property assignment works**: `(set obj.prop value)` calls setter function
- [ ] **Error handling**: Proper errors for missing/invalid properties
- [ ] **Tests pass**: Comprehensive test coverage for property operations
- [ ] **FRP ready**: Foundation supports reactive programming

## Timeline Estimate

- **Phase 1**: 1 week (HostValue extension + property access)
- **Phase 2**: 1 week (property assignment + setters)
- **Phase 3**: 1 week (testing + integration)

**Total: 3 weeks for complete native properties implementation**

## Risk Assessment

- **Evaluation Complexity**: Modifying DottedSymbol evaluation logic
- **Function Calling**: Ensuring getters/setters are called correctly
- **State Management**: Handling mutable state in functional context
- **Cross-Implementation**: Maintaining identical behavior in Haskell/Dart

## Dependencies

- **Core Evaluation**: Must support function application for getters/setters
- **IR System**: HostValue integration with IR types
- **Test Framework**: Comprehensive testing of property operations

## Property Operations Summary

### Getter Functions:
- Take 0 arguments (property access)
- Return property value as IR
- Can be computed, cached, or reactive

### Setter Functions:
- Take 1 argument (new property value)
- Return Void or confirmation
- Can validate, transform, or trigger updates

### Syntax Support:
- `obj.prop` → Property access via getter
- `(set obj.prop value)` → Property assignment via setter
- `obj.method()` → Method calls via getter returning function
