# Flutter Material Theme System

## Theme Class Reference

Extracted from: https://api.flutter.dev/flutter/material/Theme-class.html

### ThemeData Constructors

The ThemeData class provides the following constructors and factory methods that should be implemented in Glue:

#### Named Constructors (Priority: High)
- `ThemeData()` - Default constructor with all properties
- `ThemeData.from()` - Create from existing color scheme and text theme
- `ThemeData.raw()` - Raw constructor for advanced usage

#### Factory Constructors (Priority: Medium)
- `ThemeData.dark()` - Pre-configured dark theme
- `ThemeData.light()` - Pre-configured light theme
- `ThemeData.fallback()` - Fallback theme
- `ThemeData.localize()` - Localized theme

### Core Theme Properties (Priority: High)

#### ColorScheme (Most Important)
```dart
// Critical - Color scheme is the heart of theming
colorScheme: {
  primary, secondary, surface, background,
  error, onPrimary, onSecondary, onSurface, onBackground, onError,
  outline, shadow, inverseSurface, onInverseSurface,
  inversePrimary, surfaceTint
}
```

#### Typography (High Priority)
```dart
// Text themes are critical for UI consistency
textTheme: {
  displayLarge, displayMedium, displaySmall,
  headlineLarge, headlineMedium, headlineSmall,
  titleLarge, titleMedium, titleSmall,
  bodyLarge, bodyMedium, bodySmall,
  labelLarge, labelMedium, labelSmall
}
labelSmall
```

#### Component Themes (Medium Priority)
```dart
// Component-specific theming
cardTheme, appBarTheme, buttonTheme, chipTheme,
tabBarTheme, bottomNavigationBarTheme, navigationBarTheme,
navigationRailTheme, drawerTheme, dialogTheme, listTileTheme,
expansionTileTheme, tooltipTheme, snackBarTheme, bottomSheetTheme,
popupMenuTheme, dividerTheme, checkboxTheme, radioTheme,
switchTheme, sliderTheme, progressIndicatorTheme, textSelectionTheme,
inputDecorationTheme, iconTheme, primaryIconTheme, scaffoldBackgroundColor
```

#### Advanced Properties (Low Priority)
```dart
// Advanced customization options
materialTapTargetSize, pageTransitionsTheme, scrollbarTheme,
dataTableTheme, menuTheme, searchBarTheme, searchViewTheme,
actionIconTheme, extensions
```

## Glue UI Theme Implementation

### Theme Constructors for Glue

#### Primary Constructor
```clojure
(theme {:colorScheme (colorScheme {:primary primary-color
                                   :secondary secondary-color
                                   :surface surface-color})
        :textTheme (textTheme {:bodyLarge (textStyle {:color text-color})})})
```

#### Pre-configured Themes
```clojure
(theme-dark)    ; Pre-configured dark theme
(theme-light)   ; Pre-configured light theme
```

### Required Theme Support

#### ColorScheme Objects
- `colorScheme` - Main color palette
- `colorScheme-light` - Light color scheme
- `colorScheme-dark` - Dark color scheme

#### Typography Objects
- `textTheme` - Primary text styles
- `primaryTextTheme` - High-contrast text styles
- `accentTextTheme` - Secondary text styles

#### Component Themes
- `buttonTheme` - Button theming
- `cardTheme` - Card theming
- `chipTheme` - Chip theming
- `dialogTheme` - Dialog theming
- `appBarTheme` - App bar theming
- `listTileTheme` - List tile theming

## Missing Theme Support (To Implement)

### Colors Palette (High Priority)
- `Colors.red`, `Colors.blue`, `Colors.green` - Material color palette
- `ColorScheme.light()`, `ColorScheme.dark()` - Color scheme constructors

### Component Themes to Add (Medium Priority)
- TextSelectionTheme
- MaterialTapTargetSize
- ScrollbarTheme
- DataTableTheme
- ActionIconTheme

### Advanced Theming (Low Priority)
- ThemeData.copyWith() functionality
- Theme inheritance and merging
- Custom theme extensions
- AnimatedTheme support

## Implementation Priority

### Phase 1: Core Theme System (Immediate)
1. ThemeData basic constructor implementation
2. ColorScheme support
3. Basic textTheme support
4. Material color palette (Colors.x)

### Phase 2: Component Themes (Next)
1. ButtonTheme, CardTheme, ChipTheme
2. AppBarTheme, DialogTheme, ListTileTheme
3. Typography: TextTheme with all 13 text styles

### Phase 3: Advanced Themes (Future)
1. ThemeData.from() and copyWith()
2. AnimatedTheme support
3. Custom theme extensions
