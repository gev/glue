# Language Aims

## Overview

Reactor is designed as an embeddable Lisp-inspired scripting language that serves as a universal controller for domain objects, Data Transfer Objects (DTOs), and business logic in host applications.

## Core Objectives

### 1. **Universal Embeddability**
- Provide a consistent scripting interface across different host languages (Haskell, Dart, TypeScript, etc.)
- Enable seamless integration with existing application architectures
- Support FFI (Foreign Function Interface) for calling host language functions

### 2. **Safety and Reliability**
- Prevent common programming errors (null pointer exceptions, undefined behavior)
- Provide strong type safety through runtime type checking
- Ensure predictable evaluation semantics

### 3. **Expressive Power**
- Offer Lisp-inspired syntax for functional programming
- Support property objects for structured data manipulation
- Enable higher-order functions and closures
- Provide immutable data structures by default

### 4. **Domain Object Manipulation**
- Excel at operating on DTOs, dictionaries, GUI components, and business objects
- Support dot notation for property access
- Enable functional transformations of domain data

### 5. **Simplicity and Elegance**
- Minimal syntax with maximal expressiveness
- Familiar parentheses-based syntax with modern enhancements
- Clear separation between code and data (quote sugar)

## Design Philosophy

Reactor aims to be the "universal controller" that receives scripts containing domain objects and evaluates them using injected host functions. This creates a clean separation between:

- **Host Application**: Provides domain logic and data
- **Reactor Scripts**: Describe operations on that data
- **FFI Bridge**: Enables bidirectional communication

## Target Use Cases

- **Backend Services**: API request/response processing, business rule validation
- **Frontend Applications**: UI state management, form validation, component logic
- **CAD Systems**: Geometric modeling, parametric design, automation scripts
- **IoT Platforms**: Device control, sensor data processing, automation rules
- **Configuration Systems**: Programmable config with domain object manipulation

## Implementation Goals

- **Host-Language Agnostic**: Specification enables implementations in any language
- **Minimal Runtime**: Small footprint suitable for embedding
- **Extensible**: Easy to add new features and host integrations
- **Testable**: Comprehensive evaluation model for reliable execution
