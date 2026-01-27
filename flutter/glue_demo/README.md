# Glue Demo - Live UI Editor

A Flutter desktop application that demonstrates dynamic UI creation using the Glue programming language. Features a split-pane interface with a live code editor and real-time UI renderer.

## Overview

Glue Demo showcases the capabilities of the Glue programming language's UI module (`ffi.ui`) through an interactive desktop application. Users can write Glue code in the left panel and see the resulting Flutter UI components rendered in real-time on the right panel.

![Glue UI Demo Screenshot](../assets/screenshot_demo_01.png)

*The Glue Demo application interface showing live code editing and UI preview*

## Features

- **Live Code Editing**: Write Glue code with real-time evaluation
- **Split-Pane Interface**: Code editor on the left, UI preview on the right
- **Glue Syntax Examples**: Pre-loaded examples demonstrating UI components
- **Error Handling**: Clear error messages for invalid code
- **Cross-Platform Desktop**: Supports macOS, Linux, and Windows

## Demo Examples

The application includes several Glue code examples:

### Text Widget
```clojure
(text :content "Hello, Glue!"
      :color colors.blue
      :size 24
      :weight font-weight.bold)
```

### Button Widget
```clojure
(button :label "Click me!")
```

### Column Layout
```clojure
(column :children (
  (text :content "Item 1")
  (text :content "Item 2")
  (text :content "Item 3")
))
```

## Prerequisites

- Flutter SDK (3.10.7 or later)
- Dart SDK (compatible version)
- Desktop development environment (macOS, Linux, or Windows)

## Running the Application

### 1. Setup Workspace Dependencies

Ensure you're in the root Glue project directory and run:

```bash
flutter pub get
```

This will install dependencies for all workspace projects including `glue`, `glue_flutter`, and `glue_demo`.

### 2. Run on Desktop

#### macOS
```bash
cd flutter/glue_demo
flutter run --debug --device-id macos
```

#### Linux
```bash
cd flutter/glue_demo
flutter run --debug --device-id linux
```

#### Windows
```bash
cd flutter/glue_demo
flutter run --debug --device-id windows
```

### 3. Build for Distribution

#### macOS
```bash
flutter build macos
```

#### Linux
```bash
flutter build linux
```

#### Windows
```bash
flutter build windows
```

## How to Use

1. **Edit Code**: Modify the Glue code in the left panel
2. **Real-time Preview**: See UI changes instantly in the right panel
3. **Try Examples**: Uncomment and modify the example code snippets
4. **Error Handling**: Check the right panel for evaluation errors

## Architecture

- **Frontend**: Flutter desktop application with Material Design 3
- **State Management**: Flutter's built-in `ChangeNotifier` and `ValueNotifier`
- **Code Evaluation**: Pattern-based simulation of Glue code evaluation
- **UI Rendering**: Dynamic widget reconstruction based on code patterns

## Dependencies

- `glue`: Core Glue interpreter (workspace dependency)
- `glue_flutter`: Flutter UI bindings (workspace dependency)
- Flutter desktop embedding packages

## Development

### Project Structure
```
flutter/glue_demo/
├── lib/
│   └── main.dart          # Main application code
├── macos/                 # macOS platform code
├── linux/                 # Linux platform code
├── windows/               # Windows platform code
├── pubspec.yaml          # Project configuration
└── README.md             # This file
```

### Adding New Examples

Edit the `defaultCode` string in `lib/main.dart` to add new Glue code examples.

### Extending Evaluation

The current implementation uses pattern matching for demo purposes. For full Glue integration, replace the `_evaluateCode` method with actual Glue interpreter calls.

## Contributing

1. Follow the [Glue Development Rules](../../context/development-rules.md)
2. Test on all supported desktop platforms
3. Ensure code follows Flutter best practices
4. Add documentation for new features

## License

Part of the Glue project. See project LICENSE for details.

## Related Projects

- [Glue Language](https://github.com/gev/glue) - Core programming language
- [Glue Flutter Bindings](../glue_flutter/) - Flutter UI bindings
- [Glue Interpreter](../../dart/glue/) - Dart implementation
