# Glue Language Ecosystem - Implementation TODO

## Overview
This document outlines the step-by-step implementation plan for transforming Glue from a Haskell-only language into a portable, self-sustaining ecosystem with multiple host language implementations and a Glue-written standard library.

## Current Status
- ✅ Core parser (Lisp syntax with property extensions)
- ✅ Complete evaluator with environment management
- ✅ Comprehensive Haskell standard library
- ✅ Extensive test suite
- ✅ Module system (Phase 3 complete - caching, isolation, security)
- ❌ FFI framework
- ❌ Glue-written stdlib
- ❌ Documentation system
- ❌ Multi-host implementations

## Phase 1: Language Extensions (Q1 2025)

### 1.1 Implement Module and Doc Special Forms ✅
- [x] Create module special forms (`module`, `export`, `import`)
- [x] Implement evaluation-based module registration
- [x] Add module system to builtin environment
- [x] Test special form parsing and basic functionality
- [x] Complete module syntax with export/import semantics

### 1.2 Implement Module Loading System ✅
- [x] Create `Glue.Module` modules for complete module management
- [x] Implement dual-registry system: Module Registry + Imported Cache
- [x] Add global caching with lazy evaluation (JavaScript-style)
- [x] Implement dependency resolution and circular dependency detection
- [x] Add module loading with environment isolation and security
- [x] Complete Phase 3: Runtime Import System with caching

### 1.3 Add FFI Calling Syntax
- [ ] Update parser to recognize FFI calls (`(ffi-add a b)`)
- [ ] Implement FFI function lookup in environment
- [ ] Add type checking at FFI boundaries
- [ ] Create FFI validation system
- [ ] Update evaluator to handle FFI IR nodes

### 1.4 Create Documentation Parser
- [ ] Add `(doc ...)` parsing to AST
- [ ] Implement metadata extraction (params, returns, examples)
- [ ] Create documentation storage in module registry
- [ ] Add doc lookup functions
- [ ] Update parser to handle embedded documentation

## Phase 2: FFI Framework (Q1 2025)

### 2.1 Define Complete FFI Interface
- [ ] Create `Glue.FFI` module with interface specification
- [ ] Define all ~100 required FFI primitives
- [ ] Categorize FFI functions (core, math, lists, strings, objects, control, io)
- [ ] Create type definitions for FFI functions
- [ ] Document FFI interface specification

### 2.2 Implement Haskell FFI Bindings
- [ ] Create `Glue.FFI.Haskell` module
- [ ] Implement all core type functions (`ffi-type-of`, `ffi-equal`, `ffi-to-string`)
- [ ] Implement math FFI functions (`ffi-add`, `ffi-sin`, `ffi-sqrt`, etc.)
- [ ] Implement list FFI functions (`ffi-car`, `ffi-cons`, `ffi-length`, etc.)
- [ ] Implement string FFI functions (`ffi-string-concat`, `ffi-string-split`, etc.)
- [ ] Implement object FFI functions (`ffi-object-create`, `ffi-object-get`, etc.)
- [ ] Implement control flow FFI (`ffi-throw`, `ffi-catch`, `ffi-call-function`)
- [ ] Implement I/O FFI (`ffi-print`, `ffi-file-read`, `ffi-current-time`)

### 2.3 Create FFI Binding Generator
- [ ] Design template system for FFI bindings
- [ ] Create code generator for new host languages
- [ ] Implement validation against FFI specification
- [ ] Add build integration for generated bindings
- [ ] Test generator with mock implementations

### 2.4 Test FFI Integration
- [ ] Create comprehensive FFI unit tests
- [ ] Add integration tests calling FFI from Glue code
- [ ] Performance benchmark FFI calls vs direct Haskell
- [ ] Test error handling at FFI boundaries
- [ ] Validate type safety guarantees

## Phase 3: Glue Standard Library (Q2 2025)

### 3.1 Port Core Modules to Glue
- [ ] Create `stdlib/core/list.r` with FFI-based implementations
- [ ] Create `stdlib/core/math.r` with FFI math functions
- [ ] Create `stdlib/core/bool.r` with logic and control flow
- [ ] Create `stdlib/core/string.r` with string operations
- [ ] Test each module independently
- [ ] Ensure API compatibility with Haskell versions

### 3.2 Implement Utility Modules
- [ ] Create `stdlib/utils/io.r` for console and file I/O
- [ ] Create `stdlib/utils/time.r` for time/date utilities
- [ ] Create `stdlib/utils/random.r` for random number generation
- [ ] Add proper error handling and validation
- [ ] Test utility functions with FFI integration

### 3.3 Add Comprehensive Documentation
- [ ] Add `(doc ...)` forms to all stdlib functions
- [ ] Include parameter descriptions and types
- [ ] Add usage examples for each function
- [ ] Document error conditions and edge cases
- [ ] Create cross-references between related functions

### 3.4 Create Module Dependency Management
- [ ] Implement proper module initialization order
- [ ] Add namespace support for symbol resolution
- [ ] Handle module reloading and caching
- [ ] Add module metadata (version, dependencies)
- [ ] Test complex dependency scenarios

## Phase 4: Testing & Documentation (Q2 2025)

### 4.1 Build Test Framework in Glue
- [ ] Create `stdlib/test/framework.r` with `deftest`, `assert`, `describe`
- [ ] Implement test runner using module system
- [ ] Add property testing capabilities
- [ ] Create test reporting and output formatting
- [ ] Test the test framework itself

### 4.2 Port Existing Tests to Glue Syntax
- [ ] Convert Haskell Lib tests to Glue test framework
- [ ] Ensure test coverage matches current implementation
- [ ] Add integration tests for module loading
- [ ] Test FFI integration through Glue code
- [ ] Validate behavioral compatibility

### 4.3 Implement Documentation Generator
- [ ] Create `Glue.DocGen` Haskell module
- [ ] Extract embedded docs from loaded modules
- [ ] Generate HTML documentation with search
- [ ] Create API reference with cross-links
- [ ] Add command-line tool for doc generation

### 4.4 Create Cross-Implementation Test Runner
- [ ] Design test runner architecture for multiple hosts
- [ ] Implement result comparison and diffing
- [ ] Add automated testing pipeline
- [ ] Create compatibility verification suite
- [ ] Test against reference Haskell implementation

## Phase 5: Additional Implementations (Q3 2025)

### 5.1 Dart Implementation
- [ ] Set up Dart project structure
- [ ] Port Haskell AST/IR to Dart
- [ ] Implement Dart parser using Dart parsing libraries
- [ ] Create Dart evaluator with environment management
- [ ] Implement Dart FFI bindings for Flutter interop
- [ ] Test Dart implementation against Haskell reference

### 5.2 JavaScript Implementation
- [ ] Create JavaScript project with npm package
- [ ] Port core interpreter to JavaScript
- [ ] Implement JavaScript parser (consider existing JS Lisp parsers)
- [ ] Create JavaScript evaluator
- [ ] Implement direct JavaScript FFI bindings
- [ ] Test in browser and Node.js environments

### 5.3 Python Implementation (Optional)
- [ ] Set up Python package structure
- [ ] Port interpreter to Python
- [ ] Implement Python parser
- [ ] Create Python evaluator
- [ ] Implement Python FFI bindings
- [ ] Test Python implementation compatibility

## Implementation Guidelines

### Code Organization
- Keep Haskell implementation as reference
- Use consistent naming across implementations
- Maintain identical behavior across hosts
- Document host-specific considerations

### Testing Strategy
- Unit tests for each component
- Integration tests for module system
- Cross-implementation compatibility tests
- Performance regression tests
- FFI boundary validation tests

### Documentation Standards
- All functions must have embedded `(doc ...)` forms
- Include examples for common use cases
- Document error conditions and edge cases
- Keep documentation synchronized with implementation

### FFI Design Principles
- Minimal interface (~100 primitives)
- Strong typing at boundaries
- Direct host language calls for performance
- Easy extensibility for new primitives

## Success Metrics

### Language Maturity
- [ ] 100+ built-in functions across implementations
- [ ] Comprehensive test suite (>1000 tests)
- [ ] Generated documentation for all functions
- [ ] Module system with dependency resolution

### Ecosystem Health
- [ ] 3+ host language implementations
- [ ] Cross-implementation test compatibility
- [ ] Active community and contribution guidelines
- [ ] Performance benchmarks vs similar languages

### Adoption Goals
- [ ] Production-ready for scripting tasks
- [ ] Integration with major frameworks
- [ ] Educational use in programming courses
- [ ] Commercial applications and tooling

## Next Steps

1. ✅ **Module system complete** - Phase 3 implemented with caching, isolation, security
2. **Begin FFI framework development** (Phase 2.1: Define Complete FFI Interface)
3. **Implement Haskell FFI bindings** (Phase 2.2: Core FFI functions)
4. **Port core stdlib modules to Glue** (Phase 3.1: List, math, bool, string)
5. **Build test framework in Glue** (Phase 4.1: deftest, assert, describe)

The module system foundation is now complete. Next focus: FFI framework for host language interop.

This roadmap provides a clear path to a fully self-sustaining Glue ecosystem.
