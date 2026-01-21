# Glue Flutter

Flutter bindings for the Glue programming language - enabling Glue code to create and manipulate Flutter widgets.

## Overview

Glue Flutter provides a framework-agnostic UI module implementation for Flutter, allowing you to write UI code once and run it across different platforms. The same Glue code can produce Flutter widgets for mobile, desktop, and web applications.

## Features

- **Framework-Agnostic API**: Write UI code once, run everywhere
- **Native Flutter Widgets**: Direct integration with Flutter's widget system
- **Type Safety**: Full type checking through Glue's evaluation model
- **Hot Reload Support**: Compatible with Flutter's development workflow



## Supported Widgets

### Core Widgets
- `text` - Display text with styling
- `button` - Interactive buttons with callbacks
- `container` - Layout containers (Column, Row, Stack)
- `padding` - Add padding around widgets

### Layout Widgets
- `column` - Vertical layout container
- `row` - Horizontal layout container
- `center` - Center child widgets

### Advanced Widgets
- `scaffold` - Material Design scaffold
- `app-bar` - Application bar with title

## Widget Properties

### Common Properties
- `:color` - Color values ("red", "#FF0000", "rgb(255,0,0)")
- `:size` - Numeric sizes (points for mobile, pixels for web)
- `:padding` - Padding values
- `:margin` - Margin values

### Text Properties
- `:weight` - Font weight ("normal", "bold")
- `:align` - Text alignment ("left", "center", "right")

### Button Properties
- `:label` - Button text (required)
- `:on-tap` - Tap callback function
- `:disabled` - Boolean to disable interaction

### Container Properties
- `:children` - List of child widgets (required)
- `:direction` - Layout direction ("vertical", "horizontal")
- `:spacing` - Space between children
- `:align` - Child alignment

## Architecture

### Module System
Glue Flutter implements the `ffi.ui` module, providing a consistent API across different UI frameworks.

### Widget Creation
Widgets are created through native functions that:
1. Take Glue property objects
2. Parse properties into appropriate types
3. Return `IrNativeValue` containing `HostValue` with Flutter widgets

## Development

### Project Structure
```
flutter/glue_flutter/
├── lib/
│   ├── glue_flutter.dart          # Main package export
│   ├── src/
│   │   │── ui.dart                # ffi.ui module implementation
│   │   └── widgets/               # Flutter widget classes
│   │       ├── glue_widget.dart
│   │       ├── glue_text.dart
│  ...      └── ...
└── test/
    └── ui_test.dart               # Module tests
```

### Adding New Widgets

1. Create widget class in `lib/src/widgets/`
2. Add native function in `lib/src/lib/ui.dart`
3. Register in module exports
4. Add tests

## Contributing

1. Follow the Glue development rules
2. Maintain Haskell reference implementation compatibility
3. Add comprehensive tests
4. Update documentation

## License

Part of the Glue project. See project LICENSE for details.
