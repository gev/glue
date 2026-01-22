# Glue Flutter Components

This directory contains the Flutter implementation of Glue, providing a complete UI framework and development tools for building dynamic user interfaces with the Glue programming language.

## Overview

The Flutter components enable developers to:
- **Create dynamic UIs** using Glue's Lisp-inspired syntax
- **Live code editing** with real-time visual feedback
- **Build complex interfaces** through functional composition
- **Integrate with existing Flutter apps** via bindings

## Packages

### [glue_demo/](./glue_demo/)
**Live Glue UI Development Environment**

A desktop application demonstrating Glue's UI capabilities with:
- Split-pane code editor and live renderer
- Real-time evaluation and visual feedback
- Professional code editing with syntax highlighting
- Error display and debugging tools
- System theme support (dark/light mode)

**Status:** ✅ Production-ready development tool

**Technologies:**
- Flutter Desktop (macOS, Linux, Windows)
- CodeForge - Advanced code editor
- Glue Interpreter integration
- Material Design 3

### [glue_flutter/](./glue_flutter/)
**Flutter Bindings for Glue UI Framework**

Core library providing Glue-to-Flutter widget bindings:
- Complete widget library (Text, Button, Container, Column, etc.)
- Property mapping from Glue objects to Flutter widgets
- Type-safe conversions and error handling
- Performance-optimized rendering

**Status:** ✅ Stable API with comprehensive test coverage (20/20 tests passing)

**Features:**
- 100% Glue UI specification compliance
- Null-safe implementation
- Comprehensive error reporting
- Widget lifecycle management

## Architecture

```
flutter/
├── glue_demo/          # Development tool & demo app
│   ├── lib/
│   │   ├── main.dart              # App entry point
│   │   ├── widgets/               # UI components
│   │   │   ├── code_editor_pane.dart    # CodeForge integration
│   │   │   ├── ui_preview_pane.dart     # Live rendering
│   │   │   ├── error_display_widget.dart # Error visualization
│   │   │   └── widget_renderer_widget.dart # Widget display
│   │   └── services/              # Business logic
│   └── test/                      # Integration tests
│
└── glue_flutter/       # Core UI bindings
    ├── lib/
    │   ├── glue_flutter.dart     # Main API
    │   ├── src/
    │   │   ├── widgets/          # Widget implementations
    │   │   ├── utils/            # Conversion utilities
    │   │   └── types.dart        # Type definitions
    │   └── glue_flutter.dart     # Public API
    └── test/                     # Unit tests (20/20 passing)
```

## Quick Start

### Running the Demo

```bash
cd flutter/glue_demo
flutter pub get
flutter run -d macos  # or linux/windows
```

### Using Glue Flutter in Your App

```yaml
dependencies:
  glue_flutter:
    path: ../../flutter/glue_flutter
```

```dart
import 'package:glue_flutter/glue_flutter.dart';

// Create Glue runtime
final runtime = await createGlueRuntime();

// Evaluate Glue UI code
final result = await runtime.evaluate('''
(text "Hello, Glue!"
  (:color "#FF0000")
  (:size 24))
''');

// Render Flutter widget
return result.toWidget();
```

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

### Performance
- **Efficient rendering** with minimal rebuilds
- **Memory management** with proper disposal
- **Large UI support** tested with complex hierarchies
- **Smooth animations** and transitions

## Development

### Prerequisites
- Flutter SDK (3.0+)
- Dart SDK (3.0+)
- Desktop development enabled

### Building
```bash
# Build glue_demo for all platforms
cd flutter/glue_demo
flutter build macos
flutter build linux
flutter build windows

# Run tests
cd flutter/glue_flutter
flutter test
```

### Architecture Guidelines
- Follow Flutter development rules in `context/development-rules.md`
- Use BBOM avoidance principles (Big Ball of Mud)
- Single responsibility for all widgets
- Composition over inheritance
- Type safety (avoid `dynamic` and `Object`)

## Integration Points

### Glue Interpreter
- Connects to `dart/glue/` for evaluation
- Uses `envFromModules([ui])` for proper initialization
- Handles AST → IR → Widget conversion pipeline

### Workspace Management
- Defined in root `pubspec.yaml` workspace
- Shared dependencies across packages
- Version synchronization

### Documentation
- Local docs in `context/docs/`
- API references for all packages
- Integration guides and examples

## Contributing

1. **Read** `context/development-rules.md` - All contributions must follow these rules
2. **Test** thoroughly - Both unit tests and integration tests required
3. **Document** changes - Update relevant documentation
4. **Follow** Flutter guidelines - BBOM avoidance, single responsibility, type safety

### Code Organization
```
lib/
├── widgets/           # UI components (single responsibility)
├── services/          # Business logic
├── utils/            # Helper functions
└── models/           # Data structures
```

## Roadmap

### Short Term
- [ ] Advanced editor features (autocomplete, refactoring)
- [ ] Custom Glue language mode for CodeForge
- [ ] Performance profiling and optimization
- [ ] Additional widget library components

### Long Term
- [ ] Web platform support
- [ ] Mobile platform optimization
- [ ] Advanced animation and transition support
- [ ] Plugin system for custom widgets
- [ ] Visual UI builder integration

## Support

- **Issues**: Report bugs and request features
- **Documentation**: Complete API docs in `context/docs/`
- **Examples**: Working demos in `glue_demo/`
- **Tests**: 20/20 tests passing in `glue_flutter/`

## License

See root project LICENSE file.
