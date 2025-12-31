# Evaluation: Module Handling

## Overview

Module evaluation handles special forms for module registration and import. Unlike regular evaluation, modules involve state changes and lazy loading.

## Module Registration

**Input IR:** `Module exports`
**Process:** Register module metadata in evaluation state
**Output:** No runtime value (registration effect only)

### Registration Process
1. Extract module name and exports from IR
2. Store module metadata in registry
3. Module body remains unevaluated for lazy loading

### Module Metadata
- **Name:** Unique identifier for the module
- **Exports:** List of symbols exported by module
- **Body:** IR forms to evaluate when imported

## Module Import

**Input:** Import operation (handled by special forms)
**Process:** Lazy loading with caching
**Output:** Module's exported values merged into environment

### Import Process
1. Check import cache for already loaded module
2. If cached, return cached exports directly
3. If not cached:
   - Evaluate module body in isolated environment
   - Extract exported values
   - Cache results for future imports
   - Merge exports into current environment

### Isolation and Security
- Modules evaluate in separate environment
- Prevents access to importer's private variables
- Ensures clean separation between module internals and external code

## Caching Mechanism

### Import Cache
- Stores evaluated module results
- Enables one-time evaluation per module
- Shared across all importing contexts

### Cache Structure
- **Key:** Module name
- **Value:** Exported values and evaluation context
- **Persistence:** Lives for duration of evaluation session

## Export Handling

### Export Extraction
During module evaluation:
1. Execute module body forms
2. Collect defined symbols matching export list
3. Create export map of name-value pairs

### Export Merging
After successful evaluation:
1. Take exported values from cache
2. Add new environment frame with exports
3. Make exports available in importing scope

## Error Conditions

### Module Not Found
**Cause:** Import references non-existent module
**Context:** Module name and import location

### Export Not Found
**Cause:** Module doesn't export requested symbol
**Context:** Export name and module name

### Circular Import
**Cause:** Module imports itself directly or indirectly
**Context:** Import chain leading to cycle

## Lazy Evaluation Benefits

### Performance
- Modules evaluated only when imported
- Unused modules never executed
- Cached results serve multiple importers

### Startup Time
- Fast application initialization
- Modules loaded on demand
- Reduced memory footprint

### Development
- Faster iteration during development
- Only relevant modules loaded for testing
- Supports incremental loading

## Module Lifecycle

### Definition Phase
- Module forms parsed and stored
- Metadata registered in system
- No execution occurs

### Import Phase
- First import triggers evaluation
- Subsequent imports use cache
- Exports become available in scope

### Evaluation Context
- Modules evaluated in isolated environment
- Access to shared builtins and system functions
- No access to importing context variables

## Special Considerations

### Module Scope
Modules have their own evaluation context:
- Can define private functions and variables
- Only explicitly exported symbols visible externally
- Clean encapsulation of implementation details

### Re-evaluation
Modules are evaluated once per session:
- Changes require system restart
- Supports deterministic behavior
- Enables optimization opportunities

### System Modules
Built-in modules provided by runtime:
- Available without explicit loading
- Include core functions and types
- Always present in evaluation environment
