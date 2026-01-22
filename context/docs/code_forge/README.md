# CodeForge Documentation

This folder contains local documentation for the `code_forge` package (v5.2.0), extracted from the official pub.dev documentation for offline access.

## Overview

CodeForge is a powerful, feature-rich code editor widget for Flutter that provides:

- **180+ languages** syntax highlighting
- **Smart code folding** detection
- **Full LSP integration** for intelligent features
- **AI completion** support
- **Real-time semantic tokens**
- **Inline diagnostics** and error display
- **Advanced undo/redo** with smart grouping
- **Full theming support**
- **Large file handling** (tested with 100k+ lines)

## Key Classes

### CodeForge
The main code editor widget.

**Constructor:**
```dart
CodeForge({
  CodeForgeController? controller,
  FindController? findController,
  UndoRedoController? undoController,
  Map<String, TextStyle>? editorTheme,
  Mode? language,
  FocusNode? focusNode,
  TextStyle? textStyle,
  // ... more parameters
})
```

**Basic Usage:**
```dart
final controller = CodeForgeController();
controller.text = 'void main() {\n  print("Hello World");\n}';

CodeForge(
  controller: controller,
  language: langDart, // From code_forge package
  textStyle: TextStyle(fontSize: 14, fontFamily: 'monospace'),
)
```

### CodeForgeController
Manages the editor's text content, selection, and operations.

**Key Properties:**
- `text` - Get/set the full text content
- `selection` - Current text selection
- `getLineText(int line)` - Get text for specific line
- `foldAll()` / `unfoldAll()` - Code folding operations

**Basic Usage:**
```dart
final controller = CodeForgeController();
controller.text = 'your code here';

// Get selected text
String selected = controller.selection.textInside(controller.text);

// Get line content
String line0 = controller.getLineText(0);
```

## Language Support

CodeForge supports 180+ languages. For Glue development, use:

```dart
import 'package:code_forge/code_forge.dart';

// For Glue (closest is Dart syntax)
CodeForge(
  language: langDart, // Or create custom mode for Glue
  // ... other parameters
)
```

## Theming

CodeForge supports extensive theming:

```dart
CodeForge(
  editorTheme: {
    'keyword': TextStyle(color: Colors.blue, fontWeight: FontWeight.bold),
    'string': TextStyle(color: Colors.green),
    'comment': TextStyle(color: Colors.grey, fontStyle: FontStyle.italic),
    // ... more token types
  },
)
```

## Integration with Glue Demo

In the Glue demo, CodeForge replaces the basic TextField:

```dart
class CodeEditorPane extends StatefulWidget {
  final TextEditingController codeController;
  final bool isEvaluating;

  @override
  Widget build(BuildContext context) {
    return CodeForge(
      controller: codeForgeController, // Convert from TextEditingController
      language: langDart,
      textStyle: TextStyle(fontSize: 20, fontFamily: 'monospace'),
    );
  }
}
```

## Performance Features

- **Rope data structure** for efficient large text handling
- **Lazy highlighting** for better performance
- **Incremental updates** for responsive editing

## Limitations

- **No Flutter web support** (relies on `dart:io`)
- **Desktop only** platforms

## Documentation Maintenance

This documentation was extracted from:
- **Source**: https://pub.dev/documentation/code_forge/latest/
- **Version**: 5.2.0
- **Date**: Generated from live pub.dev API docs

To update this documentation:
```bash
# Download latest docs
curl -s "https://pub.dev/documentation/code_forge/latest/" > codeforge_overview.html

# Extract API information and convert to markdown
# (Manual process - update this README accordingly)
```

## Links

- **Package**: https://pub.dev/packages/code_forge
- **GitHub**: https://github.com/heckmon/code_forge
- **Live Demo**: https://pub.dev/packages/code_forge/example
