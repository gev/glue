# How to Make Glue Flutter Bindings

This guide explains how to create Flutter bindings for the Glue programming language, covering widgets, properties, and enums.

## Overview

Glue Flutter bindings provide a framework-agnostic UI API that compiles to Flutter widgets. The bindings bridge Glue's functional programming model with Flutter's widget system through:

- **Native Functions**: Curried functions that create Flutter widgets
- **Property System**: Type-safe property extraction from Glue objects
- **Enum Union Objects**: Direct access to Flutter enum values
- **Value Extractors**: Pattern matching for type-safe value conversion

## Prerequisites

Before implementing new bindings, read and understand:
- [Glue Development Rules](../../context/development-rules.md) - All implementation must follow these rules
- [UI Module Specification](../../context/ui-module-specification.md) - Framework-agnostic UI API specification

## 1. Widget Creation

### Currying Pattern

All widget functions use a consistent currying pattern for Glue's universal argument application:

```dart
// Widget function signature: (widget-name arg1 arg2 props?)
final Ir text = IrNativeFunc(textImpl);

// Step 1: Take first argument (content)
Eval<Ir> textImpl(Ir content) {
  return Eval.pure(IrNativeFunc(textWithContent(content)));
}

// Step 2: Take optional properties and create widget
Eval<Ir> Function(Ir) textWithContent(Ir content) =>
    (Ir props) => switch ((content, props)) {
      (IrString(:final value), IrObject(:final properties)) =>
        _createText(value, Properties(properties.unlock)),
      (IrString(:final value), _) => _createText(value, Properties.empty()),
      _ => throwError(wrongArgumentType(['string', 'object?'])),
    };

// Step 3: Build Flutter widget and wrap in IrNativeValue
Eval<Ir> _createText(String content, Properties properties) {
  final textWidget = Text(
    content,
    style: TextStyle(
      color: properties.color,
      fontSize: properties.size,
      fontWeight: properties.weight,
    ),
    textAlign: properties.align,
  );
  return Eval.pure(IrNativeValue(HostValue(textWidget)));
}
```

### Widget Types

#### Content Widgets (Text, Button)
- Pattern: `(widget content props?)`
- Content is required, properties optional
- Example: `(text "Hello" (:color colors.blue :size 18))`

#### Container Widgets (Column, Row, Center)
- Pattern: `(widget props)`
- Properties required (contains `:children`)
- Example: `(column (:children [child1 child2] :main-axis-alignment main-axis-alignment.center))`

## 2. Property System

### Properties Class

Create a `Properties` wrapper with lazy getters for type-safe access:

```dart
class Properties {
  final Map<String, dynamic> _props;

  Properties(Map<String, dynamic> props) : _props = props;
  Properties.empty() : _props = {};

  // Text properties
  Color? get color => extractColor(_props['color']);
  double? get size => extractDouble(_props['size']);
  FontWeight? get weight => extractFontWeight(_props['weight']);
  TextAlign? get align => extractTextAlign(_props['align']);

  // Layout properties
  List<Widget> get children => extractChildren(_props['children']) ?? [];
  MainAxisAlignment get mainAlign =>
      extractMainAxisAlignment(_props['main-axis-align']) ?? MainAxisAlignment.start;
  CrossAxisAlignment get crossAlign =>
      extractCrossAxisAlignment(_props['cross-axis-align']) ?? CrossAxisAlignment.start;

  // Button properties
  VoidCallback? get onTap => extractVoidCallback(_props['on-tap']);
  bool get disabled => extractBool(_props['disabled']) ?? false;
}
```

### Value Extractors

Use pattern matching for type-safe value extraction:

```dart
/// Extract string from Glue IR value
String? extractString(Ir value) => switch (value) {
  IrString(:final value) => value,
  _ => null,
};

/// Extract color from Glue IR value
Color? extractColor(Ir value) => switch (value) {
  IrNativeValue(value: HostValue(value: Color color)) => color,
  IrString() => parseColor(value), // Fallback for strings
  _ => null,
};

/// Extract FontWeight from Glue IR value
FontWeight? extractFontWeight(Ir value) => switch (value) {
  IrNativeValue(value: HostValue(value: FontWeight weight)) => weight,
  _ => null,
};

/// Extract children list from Glue IR value
List<Widget>? extractChildren(dynamic value) => switch (value) {
  List list => list
      .map((child) => switch (child) {
        IrNativeValue(value: HostValue(value: Widget widget)) => widget,
        _ => const SizedBox.shrink(), // Fallback for invalid children
      })
      .toList(),
  _ => null,
};
```

## 3. Enum Union Objects

### Creating Enum Objects

Enum union objects provide direct access to Flutter enum values without string parsing:

```dart
/// Colors object - Material Design colors as Glue object properties
final colors = IrObject({
  'red': IrNativeValue(HostValue(Colors.red)),
  'blue': IrNativeValue(HostValue(Colors.blue)),
  'green': IrNativeValue(HostValue(Colors.green)),
  'black': IrNativeValue(HostValue(Colors.black)),
  'white': IrNativeValue(HostValue(Colors.white)),
  // ... all Material Design colors
});

/// Font weight enum object
final fontWeight = IrObject({
  'normal': IrNativeValue(HostValue(FontWeight.normal)),
  'bold': IrNativeValue(HostValue(FontWeight.bold)),
  'w100': IrNativeValue(HostValue(FontWeight.w100)),
  'w200': IrNativeValue(HostValue(FontWeight.w200)),
  // ... all weights
});

/// Text alignment enum object
final textAlign = IrObject({
  'left': IrNativeValue(HostValue(TextAlign.left)),
  'center': IrNativeValue(HostValue(TextAlign.center)),
  'right': IrNativeValue(HostValue(TextAlign.right)),
  // ... all alignments
});
```

### Usage in Glue Code

```clojure
;; Direct enum access (type-safe, preferred)
(text "Hello World"
  (:color colors.blue)           ;; Direct Color object
  (:weight font-weight.bold)     ;; Direct FontWeight object
  (:align text-align.center))    ;; Direct TextAlign object

;; Layout with enum objects
(column
  (:cross-axis-alignment cross-axis-alignment.center)
  (:main-axis-alignment main-axis-alignment.spaceEvenly)
  (children [...]))
```

## 4. Module Registration

### Module Structure

Register all functions and objects in the native module:

```dart
/// The ui module containing all UI functions and enum objects
final ModuleInfo ui = nativeModule('ffi.ui', [
  // Core widget functions
  ('text', text),
  ('button', button),
  ('container', container),
  ('column', column),
  ('row', row),
  ('center', center),

  // Color creation functions
  ('rgb', rgb),
  ('rgba', rgba),

  // Padding creation functions
  ('padding-all', paddingAll),
  ('padding-symmetric', paddingSymmetric),

  // Enum union objects
  ('cross-axis-alignment', crossAxisAlignment),
  ('main-axis-alignment', mainAxisAlignment),
  ('text-align', textAlign),
  ('font-weight', fontWeight),
  ('colors', colors),
]);
```

### Function Creation Example

For functions that create values (like padding):

```dart
/// Padding all function - (padding-all 10)
final paddingAll = IrNativeFunc(paddingAllImpl);

Eval<Ir> paddingAllImpl(Ir value) {
  final padding = extractDouble(value);
  if (padding == null) {
    return throwError(wrongArgumentType(['number']));
  }
  return createPadding(EdgeInsets.all(padding));
}

Eval<Ir> createPadding(EdgeInsetsGeometry insets) {
  return Eval.pure(IrNativeValue(HostValue(insets)));
}
```

## 5. Testing

### Module Tests

```dart
void main() {
  group('UI Module', () {
    test('module is properly defined', () {
      expect(ui, isA<ModuleInfo>());
      expect(ui.moduleName, 'ui');
      expect(ui.exports, contains('text'));
      expect(ui.exports, contains('colors'));
    });

    test('enum objects are exported', () {
      final colorsDef = ui.definitions.firstWhere((def) => def.$1 == 'colors');
      expect(colorsDef.$2, isA<IrObject>());
    });
  });

  group('Colors Enum', () {
    test('has Material Design colors', () {
      expect((colors).properties['red'], isA<IrNativeValue>());
      expect((colors).properties['blue'], isA<IrNativeValue>());
    });

    test('values are correct Flutter colors', () {
      final redValue = ((colors).properties['red'] as IrNativeValue).value;
      expect(redValue.value, Colors.red);
    });
  });
}
```

## 6. File Organization

### Recommended Structure

```
lib/src/lib/ui/
├── ui.dart                    # Module definition
├── widgets/                   # Widget constructors
│   ├── text.dart
│   ├── button.dart
│   └── column.dart
└── styles/                    # Enum objects & utility functions
    ├── colors.dart
    ├── font_weight.dart
    ├── padding_all.dart
    └── ...

lib/src/utils/
├── value_extractors.dart      # IR value extraction
├── widget_properties.dart     # Properties wrapper
└── color_parser.dart          # String color parsing
```

## 7. Key Patterns

### Always Use HostValue Wrapping
```dart
// Correct: Wrap Flutter objects in HostValue
IrNativeValue(HostValue(Colors.red))
IrNativeValue(HostValue(textWidget))

// Incorrect: Direct object wrapping
IrNativeValue(Colors.red)
```

### Prefer Enum Objects Over Strings
```dart
// Preferred: Type-safe enum access
(:color colors.blue)

// Avoid: String parsing (if implemented)
:color "blue"
```

### Use Pattern Matching
```dart
// Preferred: Exhaustive pattern matching
Color? extractColor(Ir value) => switch (value) {
  IrNativeValue(value: HostValue(value: Color color)) => color,
  IrString() => parseColor(value),
  _ => null,
};

// Avoid: If-else chains or type casting
```

### Handle Optional Properties Gracefully
```dart
// Good: Provide sensible defaults
CrossAxisAlignment get crossAlign =>
    extractCrossAxisAlignment(_props['cross-axis-align']) ?? CrossAxisAlignment.start;

// Avoid: Force unwrapping without defaults
CrossAxisAlignment get crossAlign => extractCrossAxisAlignment(_props['cross-axis-align'])!;
```

## 8. Development Workflow

### Adding a New Widget

1. **Read the specs**: Check [UI Module Specification](../../context/ui-module-specification.md) for API requirements
2. **Follow development rules**: Ensure compliance with [Glue Development Rules](../../context/development-rules.md)
3. **Create widget file**: Add to `lib/src/lib/ui/widgets/`
4. **Implement currying pattern**: Follow the established pattern
5. **Add properties**: Update `Properties` class if needed
6. **Add value extractors**: Create extractors for new property types
7. **Register in module**: Add to `ui.dart` exports
8. **Add tests**: Create comprehensive tests
9. **Update documentation**: Update README and this guide

### Adding a New Enum Object

1. **Create enum file**: Add to `lib/src/lib/ui/styles/`
2. **Use IrObject**: Wrap all enum values in `IrNativeValue(HostValue(enumValue))`
3. **Register in module**: Add to `ui.dart` exports
4. **Add tests**: Test all enum values are accessible
5. **Update README**: Document the new enum object

This architecture provides a robust, type-safe bridge between Glue's functional UI descriptions and Flutter's widget system.
