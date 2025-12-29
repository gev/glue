# Reactor Module System Implementation TODO

## Overview
This document outlines the step-by-step implementation of Reactor's module system with eager registration and lazy evaluation.

**📚 Related Documentation:**
- [Module System Specification](MODULE_SYSTEM.md) - Complete feature overview and examples
- [Environment Architecture](MODULE_ENVIRONMENTS.md) - Detailed environment structure for imported modules

## Current Status
- ✅ **Design Complete**: Architecture, syntax, and data structures designed
- ✅ **Documentation**: Comprehensive specification in MODULE_SYSTEM.md
- ✅ **Parser Support**: Dots allowed in symbols for module names
- ✅ **Phase 1 Complete**: Core infrastructure implemented and tested
- ✅ **Phase 2 Complete**: Enhanced registration system with evaluation-based parsing

## Phase 1: Core Infrastructure ✅

### 1.1 Module Data Types
- [x] Create `Reactor/Module.hs`
- [x] Define `Module` data structure
- [x] Define `ModuleRegistry` type
- [x] Add to cabal file

### 1.2 Registration Environment
- [x] Create `Reactor/Module/Registration.hs`
- [x] Implement `module` special form (registration version)
- [x] Implement `export` special form (collects symbol names)
- [x] Implement `def` special form (registration version - no evaluation)
- [x] Create registration frame with these special forms

### 1.3 Runtime Import System
- [x] Create `Reactor/Module/Import.hs`
- [x] Implement `import` special form (runtime version)
- [x] Add to main builtin environment
- [x] Update cabal file

### 1.4 Testing & Integration
- [x] Create unit tests for module infrastructure
- [x] Separate module system from core Lib
- [x] Verify compilation and basic functionality

## Phase 2: Registration System ✅

### 2.1 Registration is Evaluation with Special Environment
- [x] **Registration = `eval moduleIR` with registration environment**
- [x] Create registration environment with special forms: `module`, `export`, `def`
- [x] `module` special form: parses structure, coordinates registration
- [x] `export` special form: collects export symbol lists
- [x] `def` special form: records symbol definitions (no evaluation)
- [x] Store complete module metadata in global registry

### 2.2 Module Parsing Logic
- [x] Parse `(module name ...)` IR structures
- [x] Extract export lists from `(export ...)` forms
- [x] Collect body forms for later evaluation
- [x] Validate module structure

### 2.3 Registry Management
- [x] Global registry storage
- [x] Registry lookup functions
- [x] Registry modification functions
- [x] Thread-safe operations

### 2.4 Testing
- [x] Unit tests for evaluation-based registration
- [x] Test export collection during evaluation
- [x] Test module registration with complex structures

## Phase 3: Runtime Import System

### 3.1 Import Evaluation
- [ ] Lookup module in registry
- [ ] Evaluate module body in isolated environment
- [ ] Extract exported symbol values
- [ ] Merge into current environment frame

### 3.2 Environment Isolation
- [ ] Create module evaluation environment
- [ ] Include builtins needed for module execution
- [ ] Prevent access to user code during module loading
- [ ] Handle module-level closures correctly

### 3.3 Symbol Merging
- [ ] Extract values for exported symbols
- [ ] Merge into current environment frame
- [ ] Handle naming conflicts
- [ ] Preserve lexical scoping

## Phase 4: Integration & Testing

### 4.1 Main Integration
- [ ] Update `Reactor/Lib.hs` to include module system
- [ ] Modify main application to load modules
- [ ] Create module loading pipeline
- [ ] Integrate with existing evaluation system

### 4.2 Test Infrastructure
- [ ] Create test modules (math, list, etc.)
- [ ] Test registration phase
- [ ] Test import functionality
- [ ] Test lexical scoping
- [ ] Test error conditions

### 4.3 Example Usage
- [ ] Create example module files
- [ ] Create example usage scripts
- [ ] Test end-to-end functionality
- [ ] Verify direct import behavior

## Phase 5: Advanced Features (Future)

### 5.1 Qualified Imports
- [ ] `(import math.utils as m)` syntax
- [ ] Qualified access: `m:add`
- [ ] Namespace management

### 5.2 Selective Imports
- [ ] `(import (add multiply) from math.utils)` syntax
- [ ] Import only specific symbols
- [ ] Rename on import

### 5.3 Documentation System
- [ ] `(doc "Description" (def func ...))` syntax
- [ ] Extract documentation from modules
- [ ] Generate API documentation

### 5.4 Module Dependencies
- [ ] Track module dependencies
- [ ] Automatic dependency loading
- [ ] Circular dependency detection

## Implementation Order

### Immediate Next Steps
1. ✅ **Phase 1 Complete** - Core infrastructure implemented
2. ✅ **Phase 2 Complete** - Enhanced registration with evaluation-based parsing
3. **Phase 3: Runtime Import System** - Improve import with environment isolation
4. **Phase 4: Integration & Testing** - End-to-end testing and examples

### Priority Order
1. Core data structures
2. Registration system (easiest to test)
3. Import system (depends on registration)
4. Integration and testing
5. Advanced features

## Testing Strategy

### Unit Tests
- [ ] Special form parsing and validation
- [ ] Registry operations
- [ ] Environment isolation
- [ ] Symbol merging

### Integration Tests
- [ ] End-to-end module loading
- [ ] Import/export verification
- [ ] Lexical scoping validation
- [ ] Error condition handling

### Example-Based Tests
- [ ] Math module with functions
- [ ] List utilities module
- [ ] Cross-module dependencies
- [ ] Complex scoping scenarios

## Success Criteria

### Functional Requirements
- [ ] `(module name (export ...) body...)` declarations work
- [ ] `(import module.name)` brings symbols into scope
- [ ] Imported symbols accessible without qualification
- [ ] Lexical scoping respected
- [ ] Multiple modules can be imported
- [ ] Module functions capture module-level variables correctly

### Non-Functional Requirements
- [ ] Efficient: Modules loaded once, cached
- [ ] Safe: Explicit exports, no accidental pollution
- [ ] Simple: No complex dependency resolution
- [ ] Extensible: Easy to add qualified imports, etc.

## Dependencies

### Required Before Implementation
- ✅ Parser supports dots in symbols
- ✅ Basic evaluation system works
- ✅ Environment/frame system implemented
- ✅ Special form infrastructure exists

### Required During Implementation
- Access to `IR` and `Eval` types
- Environment manipulation functions
- Parser for IR structures
- Error handling system

## Risk Assessment

### High Risk
- Environment isolation during module evaluation
- Symbol merging and scoping correctness
- Registry thread safety

### Medium Risk
- IR parsing for module structures
- Error handling and validation
- Performance of module loading

### Low Risk
- Data structure definitions
- Basic special form implementation
- Registry CRUD operations

## Timeline Estimate

### Phase 1 (1-2 days): Core infrastructure
### Phase 2 (2-3 days): Registration system
### Phase 3 (2-3 days): Runtime import system
### Phase 4 (1-2 days): Integration and testing
### Phase 5 (Future): Advanced features

Total: ~1-2 weeks for basic working system
