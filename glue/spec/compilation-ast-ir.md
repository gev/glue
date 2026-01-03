# AST to IR Compilation Specification

## Overview

This document describes the compilation process that transforms Glue's [Abstract Syntax Trees (AST)](ast.md) into the Intermediate Representation [IR](ir.md) used for execution.

## Compilation Process

The compilation transforms AST nodes into equivalent IR nodes, converting syntactic structure into execution-ready semantics.

## AST to IR Transformation

### Atomic AST Nodes
- `AST.String s` → `String s`
- `AST.Number n` → `Number n`
- `AST.Symbol s` → `Symbol s` (or `DottedSymbol parts` if contains dots)

### Composite AST Nodes
- `AST.List xs` → `List (map compile xs)`
- `AST.Object props` → `Object (Map.fromList (map (second compile) props))`

## Symbol Resolution

During compilation, dotted symbols in AST are split and converted to DottedSymbol IR nodes:
- AST: `Symbol "obj.field"` → IR: `DottedSymbol ["obj", "field"]`

## Relationship to IR

The resulting IR provides a unified representation optimized for runtime evaluation, as detailed in the [IR Specification](ir.md).
