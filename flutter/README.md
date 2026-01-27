# Glue Flutter Components

This directory contains the Flutter implementation of **Glue**, providing a complete UI framework and development tools for building dynamic user interfaces with the [Glue programming language](../README.md).

**Glue** is a Lisp-inspired programming language designed for functional UI development, featuring:
- Homoiconic syntax (code as data)
- Immutable data structures
- Functional composition
- Live evaluation and hot reloading
- Cross-platform UI rendering

## Overview

The Flutter components enable developers to:
- **Create dynamic UIs** using Glue's Lisp-inspired syntax
- **Live code editing** with real-time visual feedback
- **Build complex interfaces** through functional composition
- **Integrate with existing Flutter apps** via bindings

![Glue UI Demo Screenshot](assets/screenshot_demo_01.png)

*Glue UI Editor showing live code editing and real-time rendering*

## Packages

### [glue_demo/](./glue_demo/README.md)
**Live Glue UI Development Environment**

A desktop application demonstrating Glue's UI capabilities with:
- Split-pane code editor and live renderer
- Real-time evaluation and visual feedback
- Error display and debugging tools

**Status:** ✅ Production-ready development tool

**Technologies:**
- [Flutter Desktop](https://flutter.dev/desktop) (macOS, Linux, Windows)
- [CodeForge](https://pub.dev/packages/code_forge) - Advanced code editor
- [Glue Interpreter](../dart/glue/README.md) integration

### [glue_flutter/](./glue_flutter/README.md)
**Flutter Bindings for [Glue UI Framework](../context/ui-module-specification.md)**

Core library providing Glue-to-Flutter widget bindings:
- Complete widget library (Text, Button, Container, Column, etc.)
- Type-safe conversions and error handling

## Key Features

### Live Development
- **Hot reload** for instant UI updates
- **Real-time evaluation** as you type
- **Visual debugging** with error highlighting
- **Syntax highlighting** for Glue code

### Widget Library
- **Complete coverage** of UI primitives
- **Layout widgets** (Column, Row, Container)
- **Interactive elements** (Button, TextField)
- **Styling support** (colors, fonts, spacing)
- **Nested hierarchies** with proper composition

## License

See root project LICENSE file.
